import json
import subprocess
import sys

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
    StatementLabelling,
    Struct,
    SyntaxProblem,
    Var,
)
from arg2p import _jvm
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


def test_the_mcp_server():
    # as an assistant runs it: one JSON-RPC message a line, the answers too
    messages = [
        {"jsonrpc": "2.0", "id": 1, "method": "initialize", "params": {"protocolVersion": "2025-06-18", "capabilities": {}}},
        {"jsonrpc": "2.0", "method": "notifications/initialized"},
        {"jsonrpc": "2.0", "id": 2, "method": "tools/call", "params": {"name": "answer", "arguments": {"theory": CONFLICT, "query": "a"}}},
    ]
    done = subprocess.run(
        [sys.executable, "-m", "arg2p", "mcp"],
        input="".join(json.dumps(it) + "\n" for it in messages),
        capture_output=True,
        text=True,
        encoding="utf-8",
        timeout=120,
    )
    assert done.returncode == 0, done.stderr
    answers = {it["id"]: it for it in map(json.loads, done.stdout.splitlines())}
    assert answers[1]["result"]["serverInfo"]["name"] == "arg2p"
    assert answers[2]["result"]["structuredContent"]["in"] == ["a"]


def test_the_jvm_takes_the_options_of_the_environment(monkeypatch):
    monkeypatch.setenv("ARG2P_JVM_OPTIONS", " -Xmx8g  -XX:+UseG1GC ")
    assert _jvm.options()[-2:] == ["-Xmx8g", "-XX:+UseG1GC"]
    monkeypatch.delenv("ARG2P_JVM_OPTIONS")
    assert _jvm.options() == [f"-Xss{_jvm.STACK}", "-Dslf4j.internal.verbosity=ERROR"]


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
