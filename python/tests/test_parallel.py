import time

from arg2p import Cluster, Flags, Solver

THEORY = "r1 :=> a.\nr2 : a => b.\nr3 :=> c.\nr4 : c => -b.\nsup(r2, r4).\nr5 :=> d.\n"


def claims(evaluation):
    return {label: sorted(e.claim for e in getattr(evaluation, label)) for label in ("in_", "out", "und")}


def test_a_cluster_labels_as_the_standard_solver():
    solver = Solver(THEORY)
    with Cluster(threads=2) as cluster:
        cluster.load(THEORY)
        for statement in ("b", "-b", "d"):
            answer = solver.answer(statement)
            labels = claims(cluster.solve(statement))
            assert [len(answer.in_), len(answer.out), len(answer.und)] == [len(labels[l]) for l in ("in_", "out", "und")]


def test_clusters_federated_over_websockets():
    with Cluster(threads=1, flags=Flags(argument_labelling_mode="grounded")) as evaluating, Cluster(threads=2) as lending:
        port = evaluating.listen(0, "127.0.0.1")
        lending.connect(f"ws://127.0.0.1:{port}")
        deadline = time.time() + 10
        while evaluating.peers != {lending.site}:
            assert time.time() < deadline
            time.sleep(0.02)
        evaluating.load(THEORY)
        assert claims(evaluating.solve("b"))["in_"] == ["b"]
        assert claims(evaluating.solve("-b"))["out"] == ["-b"]


def test_a_theory_changes():
    with Cluster(threads=2) as cluster:
        cluster.load("f1 :=> a.")
        assert claims(cluster.solve("a"))["in_"] == ["a"]
        cluster.add("f2 :=> -a")
        assert claims(cluster.solve("a"))["und"] == ["a"]
        cluster.reset()
        assert claims(cluster.solve("a")) == {"in_": [], "out": [], "und": ["a"]}
