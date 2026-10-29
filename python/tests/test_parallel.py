import threading
import time

import pytest

from arg2p import Arg2p, Arg2pError, Flags, LocalCluster, Semantics, ThreadedCluster

THEORY = "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => -b.\nsup(r2, r4).\nr5 :=> d.\n"


def claims(response):
    return {label: sorted(it.claim for it in getattr(response, label)) for label in ("in_args", "out_args", "und_args")}


def test_a_cluster_labels_as_the_standard_solver():
    arg2p = Arg2p.of(THEORY)
    with ThreadedCluster(threads=2) as cluster:
        cluster.load(THEORY)
        for statement in ("b", "-b", "d"):
            answer = arg2p.answer(statement)
            labels = claims(cluster.solve(statement))
            expected = [len(answer.in_statements), len(answer.out_statements), len(answer.und_statements)]
            assert expected == [len(labels[label]) for label in ("in_args", "out_args", "und_args")]


def test_a_local_cluster():
    cluster = LocalCluster(parts=2)
    cluster.load(THEORY)
    response = cluster.solve("b")
    assert claims(response)["in_args"] == ["b"]
    assert response.in_args[0].query_chain


def test_clusters_federated_over_websockets():
    flags = Flags.defaults().argument_labelling_mode(Semantics.GROUNDED)
    with ThreadedCluster(threads=1, flags=flags) as evaluating, ThreadedCluster(threads=2) as lending:
        port = evaluating.listen(0, "127.0.0.1")
        lending.connect(f"ws://127.0.0.1:{port}")
        deadline = time.time() + 10
        while evaluating.peers != {lending.id}:
            assert time.time() < deadline
            time.sleep(0.02)
        evaluating.load(THEORY)
        assert claims(evaluating.solve("b"))["in_args"] == ["b"]
        assert claims(evaluating.solve("-b"))["out_args"] == ["-b"]


def test_a_theory_changes():
    with ThreadedCluster(threads=2) as cluster:
        cluster.load("f1 :=> a.")
        assert claims(cluster.solve("a"))["in_args"] == ["a"]
        cluster.add("f2 :=> -a")
        assert claims(cluster.solve("a"))["und_args"] == ["a"]


def test_a_statement_that_cannot_be_read_is_an_error():
    cluster = LocalCluster()
    cluster.load(THEORY)
    with pytest.raises(Arg2pError, match="Not a statement"):
        cluster.solve("p(")
    assert claims(cluster.solve("b"))["in_args"] == ["b"]
    with ThreadedCluster(threads=1) as threaded:
        threaded.load(THEORY)
        with pytest.raises(Arg2pError):
            threaded.solve("p(")


def test_a_cluster_discovers_the_others_on_the_local_network():
    with ThreadedCluster(threads=1) as cluster:
        cluster.discover()


def test_a_cluster_is_a_solver_of_arg2p():
    arg2p = Arg2p.of(THEORY)
    with ThreadedCluster(threads=2) as cluster:
        cluster.load(THEORY)
        solver = cluster.solver
        expected = arg2p.solve("buildLabelSets(In, Out, Und)").next()
        solution = solver.solve("buildLabelSets(In, Out, Und)").next()
        assert [expected.text(it) for it in ("In", "Out", "Und")] == [solution.text(it) for it in ("In", "Out", "Und")]
        assert sorted(it.label for it in arg2p.graph().labellings) == sorted(it.label for it in solver.graph().labellings)
        assert solver.answer("b").in_statements == arg2p.answer("b").in_statements


def test_the_statistics_of_a_cluster():
    cluster = LocalCluster(parts=2)
    assert cluster.stats.evaluation is None
    cluster.load(THEORY)
    cluster.solve("b")
    stats = cluster.stats
    assert [it.id for it in stats.sites] == [stats.site]
    assert stats.evaluation.locality == "directional"
    assert stats.evaluation.added == sum(it.arguments for it in stats.evaluation.workers)


def test_the_flags_of_a_cluster_change():
    cluster = LocalCluster(parts=2)
    cluster.load(THEORY)
    flags = Flags.defaults().argument_labelling_mode(Semantics.COMPLETE).query_mode(False)
    cluster.flags = flags
    assert "queryMode" not in str(cluster.flags.jvm().toString())
    expected = Arg2p.of(THEORY, flags).solve("buildLabelSets(In, Out, Und)").next()
    assert str(expected) == str(cluster.solver.solve("buildLabelSets(In, Out, Und)").next())


def test_a_goal_is_stopped():
    with ThreadedCluster(threads=2) as cluster:
        cluster.load("slow :- between(1, 100000000, _), fail.\nslow.\n")
        solver = cluster.solver
        threading.Timer(0.5, solver.stop).start()
        solution = solver.solve("slow").next()
        assert solution.is_halt and solution.error == "The evaluation was stopped"
        cluster.load(THEORY)
        assert claims(cluster.solve("b"))["in_args"] == ["b"]
