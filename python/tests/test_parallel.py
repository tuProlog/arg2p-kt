import threading
import time

import pytest

from arg2p import Arg2p, Arg2pError, Federation, Flags, Semantics, ThreadedSolver

THEORY = "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => -b.\nsup(r2, r4).\nr5 :=> d.\n"


def claims(response):
    return {label: sorted(it.claim for it in getattr(response, label)) for label in ("in_args", "out_args", "und_args")}


def test_a_solver_labels_as_the_standard_solver():
    arg2p = Arg2p.of(THEORY)
    with ThreadedSolver(threads=2) as parallel:
        parallel.load(THEORY)
        for statement in ("b", "-b", "d"):
            (answer,) = arg2p.answer(statement)
            labels = claims(parallel.solve(statement))
            expected = [len(answer.in_statements), len(answer.out_statements), len(answer.und_statements)]
            assert expected == [len(labels[label]) for label in ("in_args", "out_args", "und_args")]


def test_a_solver_of_two_parts():
    parallel = ThreadedSolver(threads=2, parts=2)
    parallel.load(THEORY)
    response = parallel.solve("b")
    assert claims(response)["in_args"] == ["b"]
    assert response.in_args[0].query_chain


def test_solvers_federated_over_websockets():
    flags = Flags.defaults().argument_labelling_mode(Semantics.GROUNDED)
    with ThreadedSolver(threads=1, flags=flags) as evaluating, ThreadedSolver(threads=2) as lending:
        federation = Federation(evaluating)
        port = federation.listen(0, "127.0.0.1")
        Federation(lending).connect(f"ws://127.0.0.1:{port}")
        deadline = time.time() + 10
        while federation.peers != {lending.id}:
            assert time.time() < deadline
            time.sleep(0.02)
        evaluating.load(THEORY)
        assert claims(evaluating.solve("b"))["in_args"] == ["b"]
        assert claims(evaluating.solve("-b"))["out_args"] == ["-b"]


def test_a_theory_changes():
    with ThreadedSolver(threads=2) as parallel:
        parallel.load("f1 :=> a.")
        assert claims(parallel.solve("a"))["in_args"] == ["a"]
        parallel.add("f2 :=> -a")
        assert claims(parallel.solve("a"))["und_args"] == ["a"]


def test_a_statement_that_cannot_be_read_is_an_error():
    parallel = ThreadedSolver(threads=1)
    parallel.load(THEORY)
    with pytest.raises(Arg2pError, match="Not a statement"):
        parallel.solve("p(")
    assert claims(parallel.solve("b"))["in_args"] == ["b"]
    with ThreadedSolver(threads=1) as threaded:
        threaded.load(THEORY)
        with pytest.raises(Arg2pError):
            threaded.solve("p(")


def test_a_solver_is_a_solver_of_arg2p():
    arg2p = Arg2p.of(THEORY)
    with ThreadedSolver(threads=2) as parallel:
        parallel.load(THEORY)
        solver = parallel.goals
        expected = arg2p.solve("solve(In, Out, Und)").next()
        solution = solver.solve("solve(In, Out, Und)").next()
        assert [expected.text(it) for it in ("In", "Out", "Und")] == [solution.text(it) for it in ("In", "Out", "Und")]
        assert sorted(it.label for it in arg2p.graph().labellings) == sorted(it.label for it in solver.graph().labellings)
        assert solver.answer("b") == arg2p.answer("b")


def test_the_statistics_of_a_solver():
    parallel = ThreadedSolver(threads=2, parts=2)
    assert parallel.evaluation is None
    parallel.load(THEORY)
    parallel.solve("b")
    assert parallel.evaluation.locality == "directional"
    assert parallel.evaluation.added == sum(it.arguments for it in parallel.evaluation.workers)
    stats = Federation(parallel).stats
    assert [it.id for it in stats.sites] == [stats.site] == [parallel.id]


def test_the_flags_of_a_solver_change():
    parallel = ThreadedSolver(threads=2, parts=2)
    parallel.load(THEORY)
    flags = Flags.defaults().argument_labelling_mode(Semantics.COMPLETE).query_mode(False)
    parallel.flags = flags
    assert "queryMode(off)." in str(parallel.flags.jvm().toString())
    expected = Arg2p.of(THEORY, flags).solve("solve(In, Out, Und)").next()
    assert str(expected) == str(parallel.goals.solve("solve(In, Out, Und)").next())


def test_a_goal_is_stopped():
    with ThreadedSolver(threads=2) as parallel:
        parallel.load("slow :- between(1, 100000000, _), fail.\nslow.\n")
        solver = parallel.goals
        threading.Timer(0.5, solver.stop).start()
        solution = solver.solve("slow").next()
        assert solution.is_halt and solution.error == "The evaluation was stopped"
        parallel.load(THEORY)
        assert claims(parallel.solve("b"))["in_args"] == ["b"]


def test_a_connection_presents_the_code():
    with ThreadedSolver(threads=1) as first, ThreadedSolver(threads=1) as admitted:
        listening = Federation(first)
        listening.requires_code = True
        assert len(listening.code) == 4
        port = listening.listen(0, "127.0.0.1")
        connecting = Federation(admitted)
        with pytest.raises(Arg2pError):
            connecting.connect(f"ws://127.0.0.1:{port}", "0000" if listening.code != "0000" else "0001")
        connecting.connect(f"ws://127.0.0.1:{port}", listening.code)
        deadline = time.time() + 10
        while listening.peers != {admitted.id}:
            assert time.time() < deadline
            time.sleep(0.02)


def test_a_compact_graph_of_a_solver():
    with ThreadedSolver(threads=2) as parallel:
        parallel.load(THEORY)
        solver = parallel.goals
        (graph,) = solver.graphs()
        (compact,) = solver.compact_graphs()
        assert compact.size == len(graph.arguments)
        assert len(compact.attackers) == len(graph.attacks)
        assert sorted(compact.descriptor(it) for it in range(compact.size)) == sorted(it.descriptor for it in graph.arguments)
