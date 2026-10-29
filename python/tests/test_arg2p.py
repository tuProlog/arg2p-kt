import io
import json
import os
import subprocess
import sys
import time

import pytest

from arg2p import (
    Answer,
    Arg2p,
    Arg2pError,
    ConflictModel,
    Flags,
    GraphBuilder,
    GraphExtension,
    GraphLanguage,
    OrderingComparator,
    OrderingPrinciple,
    Profile,
    Semantics,
    Solutions,
    StatementLabelling,
    Struct,
    SyntaxProblem,
    Var,
    page,
)
from arg2p import _jvm, recorded
from arg2p.__main__ import main

CONFLICT = "r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2)."


def test_a_query_is_answered():
    answer = Arg2p.of(CONFLICT).answer("a")
    assert answer.in_statements == ["a"]
    assert answer.label("a") == "in"
    negated = Arg2p.of(CONFLICT).answer("-a")
    assert [str(it) for it in negated.out_statements] == ["-a"]
    assert negated.out_statements == [Struct("-", ("a",))]
    assert negated.label("-a") == "out"
    # a statement is compared as a term: '1' is not 1
    assert Arg2p.of("f1 :=> p(1).").answer("p(X)").label("p('1')") is None


def test_the_graph_of_each_labelling():
    (graph,) = Arg2p.of(CONFLICT).graphs()
    assert [it.descriptor for it in graph.arguments] == ["A0 : r2 : -a", "A1 : r1 : a"]
    assert graph.conclusions("in") == ["a"]
    assert graph.conclusions("out") == ["-a"]
    (attack,) = graph.attacks
    assert (attack.attacker.identifier, attack.target.identifier, attack.type) == ("A1", "A0", "rebut")
    assert graph.label(attack.attacker) == "in"


def test_a_semantics_with_several_labellings_gives_a_graph_for_each():
    flags = Flags.defaults().argument_labelling_mode(Semantics.PREFERRED)
    assert len(Arg2p.of("f1 :=> d.\nf2 :=> -d.", flags).graphs()) == 2


def test_the_graph_of_the_last_resolution():
    arg2p = Arg2p.of(CONFLICT)
    assert arg2p.solve("buildLabelSets").next().is_yes
    assert arg2p.graph() == arg2p.graph_at(arg2p.active_branch)
    assert len(arg2p.graph().arguments) == 2
    assert arg2p.context_facts(arg2p.active_branch, "statIn(S)").next().is_yes


def test_the_solutions_of_a_goal_bind_its_variables():
    # the last solution is no: there are no others
    solutions = [it for it in Arg2p.of().solve("member(X, [a, f(-b), [1, 2.5], Y])") if it.is_yes]
    assert [it.text("X") for it in solutions] == ["a", "f(-b)", "[1, 2.5]", "Y"]
    assert solutions[0].value("X") == "a"
    assert solutions[2].bindings == {"X": [1, 2.5]}
    assert solutions[3].value("X") == Var("Y")
    assert solutions[0].variables == ["X"]
    assert not Arg2p.of().solve("member(x, [])").next().is_yes
    assert len(Arg2p.of().solve("member(X, [a, b, c])").to_list(limit=2)) == 2


class _Failing:
    """Solutions of the JVM whose every step fails with an exception of arg2p, as those of a cluster may"""

    def __init__(self, exception):
        self.exception = exception

    def hasNext(self):  # noqa: N802 - the name of the JVM
        raise self.exception

    def next(self):
        raise self.exception

    def toList(self, limit):  # noqa: N802 - the name of the JVM
        raise self.exception


def test_an_exception_of_arg2p_computing_a_solution_is_an_arg2p_error():
    exception = _jvm.jclass("it.unibo.tuprolog.argumentation.Arg2pException")("The evaluation failed")
    solutions = Solutions(_Failing(exception))
    for step in (solutions.has_next, solutions.next, solutions.to_list, lambda: next(solutions)):
        with pytest.raises(Arg2pError, match="The evaluation failed"):
            step()


def test_an_error_halts_the_solution_and_is_raised_by_an_answer():
    # a rule concludes an assumption: the theory is not flat ABA
    non_flat = "assumption(a).\nassumption(b).\ncontrary(a, x).\ncontrary(b, y).\nr0 : b -> a."
    with pytest.raises(Arg2pError, match="not flat"):
        Arg2p.of(non_flat, Flags.defaults().graph_build_mode(Profile.ABA)).answer("a")
    assert Arg2p.of().solve("throw(error(type_error(integer, a), x))").next().is_halt
    with pytest.raises(Arg2pError):
        Arg2p.of().solve("f(")


def test_texts_that_do_not_parse_tell_where():
    with pytest.raises(Arg2pError) as raised:
        Arg2p.of("a :=> b.\nc :=> d(.")
    problem = raised.value.syntax
    assert problem == SyntaxProblem(2, 9, ".", "Unexpected token '.'; expected term")
    assert problem.length == 1
    assert Arg2p.check_theory("a :=> b.\nc :=> d(.") == problem
    assert Arg2p.check_theory("graphBuildMode(abstract).\na <-> b.") is None
    assert Arg2p.check_query("buildLabelSets(In, Out, Und)") is None
    assert Arg2p.check_query("flies(").line == 1


def test_the_theory_changes():
    arg2p = Arg2p.of("f1 :=> a.")
    arg2p.add("f2 :=> b.")
    assert arg2p.answer("b").in_statements == ["b"]
    arg2p.load("f3 :=> c.")
    assert arg2p.answer("b") == Answer()
    assert "f3" in arg2p.theory


def test_what_the_solver_prints_goes_to_its_output():
    printed = []
    arg2p = Arg2p.builder().output(printed.append).build()
    arg2p.solve("write(hello)").next()
    assert "".join(printed) == "hello"


def test_flags_are_facts():
    assert str(Flags.none().argument_labelling_mode(Semantics.PREFERRED)) == "argumentLabellingMode(preferred).\n"
    assert str(Flags.none().graph_extensions().query_mode(True)) == "graphExtension(none).\nqueryMode.\n"
    assert Flags.none().graph_build_mode(Profile.ABA_PLUS) == Flags.parse("graphBuildMode(aba_plus).")
    assert "graphBuildMode(aspic_plus)." in str(Flags.defaults())


@pytest.mark.parametrize(
    "values",
    [Profile, Semantics, StatementLabelling, OrderingPrinciple, OrderingComparator, GraphExtension, GraphLanguage,
     GraphBuilder, ConflictModel],
)
def test_the_values_of_the_flags_are_those_of_arg2p(values):
    kotlin = _jvm.jclass(f"it.unibo.tuprolog.argumentation.{values.__name__}").values()
    assert {(str(it.name()), str(it.getValue())) for it in kotlin} == {(it.name, it.value) for it in values}


def test_the_flags_of_the_solver_are_the_ones_it_uses():
    theory = "f1 :=> d.\nf2 :=> -d."
    assert Arg2p.of(theory).answer("d").und_statements == ["d"]
    stable = Arg2p.of(theory, Flags.defaults().argument_labelling_mode(Semantics.STABLE)).graphs()
    assert sorted(graph.conclusions("in") for graph in stable) == [["-d"], ["d"]]


def test_the_causality_of_the_theory():
    causality = Arg2p.of("r1 :=> a.\nr2 : a => b.").causality
    assert [str(it) for it in causality.ness("a", "b")] == ["-a"]
    assert causality.but_for("a", "b")
    assert causality.ness_intervention("[-a]", "b")
    assert causality.ness("b", "a") is None
    assert not causality.but_for("b", "a")


def test_the_version():
    assert Arg2p.version()[0].isdigit()


def test_the_command_line(tmp_path, capsys):
    theory = tmp_path / "theory.pl"
    theory.write_text(CONFLICT)
    assert main([str(theory), "-q", "a"]) == 0
    assert capsys.readouterr().out.strip() == "a: in [a], out [], und []"
    assert main([str(theory), "-s", "preferred"]) == 0
    assert "in: a" in capsys.readouterr().out


def test_a_file_the_command_line_cannot_read_or_write_is_told(tmp_path, capsys):
    missing = str(tmp_path / "none.pl")
    for command in ([missing], ["page", missing]):
        assert main(command) == 1
        assert capsys.readouterr().err.startswith(f"arg2p: cannot read {missing}: ")
    (tmp_path / "latin.pl").write_bytes("f1 :=> caf\xe9.".encode("latin-1"))
    assert main([str(tmp_path / "latin.pl")]) == 1
    assert "not UTF-8" in capsys.readouterr().err
    theory = tmp_path / "theory.pl"
    theory.write_text(CONFLICT)
    unwritable = str(tmp_path / "no" / "such" / "page.html")
    assert main(["page", str(theory), "-o", unwritable]) == 1
    assert capsys.readouterr().err.startswith(f"arg2p: cannot write {unwritable}: ")


def test_the_mcp_server():
    # as an assistant runs it: one JSON-RPC message a line, the answers too; the input closed once they are answered
    messages = [
        {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2025-06-18", "capabilities": {}}},
        {"jsonrpc": "2.0", "method": "notifications/initialized"},
        {"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "answer", "arguments": {"theory": CONFLICT, "query": "a"}}},
    ]
    server = subprocess.Popen(
        [sys.executable, "-m", "arg2p", "mcp"],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        text=True,
        encoding="utf-8",
    )
    try:
        server.stdin.write("".join(json.dumps(it) + "\n" for it in messages))
        server.stdin.flush()
        answers = {}
        while len(answers) < 2:
            answer = json.loads(server.stdout.readline())
            answers[answer["id"]] = answer
        server.stdin.close()
        assert server.wait(timeout=60) == 0
    finally:
        server.kill()
    assert answers[1]["result"]["serverInfo"]["name"] == "arg2p"
    assert answers[2]["result"]["structuredContent"]["in"] == ["a"]


def test_a_command_run_by_the_command_line_gives_its_exit_status():
    # in place of the process on POSIX: in a process of its own
    command = [sys.executable, "-c", "import sys; sys.exit(3)"]
    script = f"import sys; from arg2p.__main__ import _run; sys.exit(_run({command!r}))"
    assert subprocess.run([sys.executable, "-c", script], timeout=60).returncode == 3


@pytest.mark.skipif(sys.platform != "win32", reason="on POSIX, the command is the process itself")
def test_a_command_run_by_the_command_line_is_killed_with_it_on_windows():
    # the command tells its process, then waits: killing the command line kills it too
    command = [sys.executable, "-c", "import os, time; print(os.getpid(), flush=True); time.sleep(120)"]
    script = f"import sys; from arg2p.__main__ import _run; sys.exit(_run({command!r}))"
    wrapper = subprocess.Popen([sys.executable, "-c", script], stdout=subprocess.PIPE, text=True)
    child = int(wrapper.stdout.readline())
    wrapper.kill()
    wrapper.wait(timeout=60)

    def running():
        listed = subprocess.run(["tasklist", "/FI", f"PID eq {child}", "/NH"], capture_output=True, text=True).stdout
        return str(child) in listed

    deadline = time.monotonic() + 30
    while running() and time.monotonic() < deadline:
        time.sleep(0.2)
    assert not running()


def test_the_command_line_tells_of_the_mcp_server(capsys):
    with pytest.raises(SystemExit) as done:
        main(["--help"])
    assert done.value.code == 0
    out = " ".join(capsys.readouterr().out.split())
    assert "arg2p mcp" in out
    assert "arg2p page" in out


def test_arg2p_mcp_runs_the_server_and_a_theory_named_mcp_is_dot_slash_mcp(tmp_path, monkeypatch, capsys):
    from arg2p._version import MCP_SERVER

    import arg2p.__main__ as command_line

    commands = []
    monkeypatch.setattr(command_line, "_run", lambda command: commands.append(command) or 0)
    assert main(["mcp"]) == 0
    assert commands == [[_jvm.java(), *_jvm.options(), "-cp", os.pathsep.join(_jvm.classpath()), MCP_SERVER, "--mcp"]]
    monkeypatch.chdir(tmp_path)
    (tmp_path / "mcp").write_text(CONFLICT)
    assert main(["./mcp", "-q", "a"]) == 0
    assert capsys.readouterr().out.strip() == "a: in [a], out [], und []"
    assert len(commands) == 1


TWEETY = "f1 :=> bird(tweety).\nr1 : bird(X) => flies(X).\nf2 :=> -flies(tweety)."


def _recording(html):
    script = '<script type="application/json" id="recording">'
    return json.loads(html.split(script, 1)[1].split("</script>", 1)[0])


def test_the_recorded_page_of_a_run():
    html = page(TWEETY, "buildLabelSets(In, Out, Und)", Flags.parse("argumentLabellingMode(grounded)."))
    recording = _recording(html)
    assert recording["theory"] == TWEETY
    assert recording["solve"][-1]["type"] == "started"
    (solution,) = [it["solution"] for answers in recording["next"] for it in answers if it["type"] == "solution"]
    labels = {it["conclusion"]: it["label"] for it in solution["graph"]["arguments"]}
    assert labels["-flies(tweety)"] == "in" and labels["flies(tweety)"] == "out"
    assert recording["settings"]["argumentLabellingMode"] == ["grounded"]
    assert "<title>Arg2P · buildLabelSets(In, Out, Und)</title>" in html
    # a theory that does not parse is recorded as such
    assert _recording(page("f1 :=> bird(tweety"))["solve"][-1]["type"] == "failed"


def test_the_help_of_arg2p_page_tells_the_defaults_of_the_recorded_page(capsys):
    with pytest.raises(SystemExit):
        main(["page", "--help"])
    # argparse wraps the lines, a long text anywhere: compared without their blanks
    out = "".join(capsys.readouterr().out.split())
    defaults = recorded.defaults()
    for expected in (
        f"(default: {defaults['query']})",
        f"(default: {defaults['flags']})".replace("\n", "\\n"),
        f"how many solutions at most (default: {defaults['max_solutions']})",
        f"how long to solve, in seconds (default: {defaults['timeout']})",
    ):
        assert "".join(expected.split()) in out


def test_arg2p_page_writes_the_page(tmp_path, monkeypatch, capsys):
    monkeypatch.chdir(tmp_path)
    (tmp_path / "tweety.pl").write_text(TWEETY)
    flags = "argumentLabellingMode(complete).\\nqueryMode."  # \n a new line, as typed
    assert main(["page", "tweety.pl", "-q", "flies(X)", "-f", flags, "--max-solutions", "1"]) == 0
    written = tmp_path / "arg2p-tweety.html"
    assert capsys.readouterr().out.strip() == str(written)
    recording = _recording(written.read_text(encoding="utf-8"))
    assert (recording["query"], recording["flags"]) == ("flies(X)", "argumentLabellingMode(complete).\nqueryMode.")
    assert recording["solve"][-1]["type"] == "started"
    monkeypatch.setattr(sys, "stdin", io.StringIO(TWEETY))
    assert main(["page", "-", "-o", "explore.html"]) == 0
    assert _recording((tmp_path / "explore.html").read_text(encoding="utf-8"))["theory"] == TWEETY


def test_the_jvm_takes_the_options_of_the_environment(monkeypatch):
    monkeypatch.setenv("ARG2P_JVM_OPTIONS", " -Xmx8g  -XX:+UseG1GC ")
    assert _jvm.options()[-2:] == ["-Xmx8g", "-XX:+UseG1GC"]
    # quoted, an option with blanks
    monkeypatch.setenv("ARG2P_JVM_OPTIONS", '-Xmx8g "-Dlog=/my logs/arg2p.log"')
    assert _jvm.options()[-2:] == ["-Xmx8g", "-Dlog=/my logs/arg2p.log"]
    monkeypatch.delenv("ARG2P_JVM_OPTIONS")
    assert _jvm.options() == [f"-Xss{_jvm.STACK}", "-Dslf4j.internal.verbosity=ERROR"]


def test_options_split_as_a_shell_splits_them():
    # a backslash separates directories on Windows, and escapes a character elsewhere
    assert _jvm.split(r'"-Dlog=C:\My logs\a.log" -Xmx1g', windows=True) == [r"-Dlog=C:\My logs\a.log", "-Xmx1g"]
    assert _jvm.split(r"-Dlog=/my\ logs/a.log -Xmx1g", windows=False) == ["-Dlog=/my logs/a.log", "-Xmx1g"]


def test_the_jars_of_the_environment_come_before_those_of_the_package(monkeypatch, tmp_path):
    stated = [str(tmp_path / "a.jar"), str(tmp_path / "b.jar")]
    monkeypatch.setenv("ARG2P_CLASSPATH", os.pathsep.join(stated))
    jars = _jvm.classpath()
    assert jars[:2] == stated
    assert any("lsp" in os.path.basename(it) for it in jars[2:])  # the MCP server, which they lack


def test_java_is_that_of_the_runtime_of_the_jvm(monkeypatch, tmp_path):
    executable = "java.exe" if sys.platform == "win32" else "java"
    jdk = tmp_path / "jdk"
    (jdk / "bin" / "server").mkdir(parents=True)
    (jdk / "bin" / executable).write_text("")
    monkeypatch.setenv("ARG2P_JVM", str(jdk / "bin" / "server" / "jvm.dll"))
    assert _jvm.java() == str(jdk / "bin" / executable)
    # a library with no java above it: the one on the path
    monkeypatch.setenv("ARG2P_JVM", str(tmp_path / "none" / "lib" / "server" / "libjvm.so"))
    assert _jvm.java() == "java"


def test_the_settings_in_effect_come_from_the_theory_the_flags_and_the_profile():
    aspic = Arg2p.of("r1 : [] => a.")
    assert "graphBuildMode" in aspic.known_flags
    assert aspic.setting("graphBuildMode") == ["aspic_plus"]
    assert set(aspic.setting("graphExtension")) == {"standardPref", "rebutRestriction"}
    assert aspic.setting("autoTransposition") == ["true"]
    abstract = Arg2p.of("graphBuildMode(abstract).\na -> b.")
    assert abstract.setting("queryMode") == []
    with pytest.raises(Arg2pError):
        aspic.setting("noSuchFlag")
