import pytest

from arg2p import Arg2pError, Flags, Solver, Struct, Var, answer_query, evaluate

PREFERRED = "r1 : [] => a.\nr2 : [] => -a.\nsup(r1, r2).\n"


def test_a_query_is_answered_by_label():
    solver = Solver(PREFERRED)
    assert solver.answer("a").in_ == ["a"]
    assert [str(s) for s in solver.answer("-a").out] == ["-a"]
    assert answer_query(PREFERRED, "a").und == []


def test_the_profile_of_the_flags_reads_the_theory():
    # aspic weighs no preference: a and -a rebut each other
    assert answer_query(PREFERRED, "a", Flags(graph_build_mode="aspic")).und == ["a"]
    # nor does the theory once it states the profile
    assert answer_query(PREFERRED + "graphBuildMode(aspic).", "a").und == ["a"]


def test_a_theory_the_profile_refuses_raises():
    aba = "graphBuildMode(aba).\nassumption(a).\ncontrary(a, x).\nr1 : x -> a.\n"
    with pytest.raises(Arg2pError):
        Solver(aba).answer("a")


def test_goals_bind_python_values():
    (solution,) = Solver().solve("X = f(Y, [1, 2.5, b], -c)")
    value = solution["X"]
    assert isinstance(value, Struct) and value.functor == "f"
    assert isinstance(value.args[0], Var)
    assert value.args[1] == [1, 2.5, "b"]
    assert str(value.args[2]) == "-c"
    assert len(Solver().solve("member(X, [a, b, c])", limit=2)) == 2


def test_a_goal_that_cannot_be_parsed_raises():
    with pytest.raises(Arg2pError):
        Solver().solve("f(")


def test_each_labelling_has_its_graph():
    theory = "f1 :=> d.\nf2 :=> -d.\n"
    (grounded,) = evaluate(theory)
    assert grounded.conclusions("und") == {"d", "-d"}
    assert {(a.type, a.attacker, a.target) for a in grounded.attacks} == {("undermine", "A0", "A1"), ("undermine", "A1", "A0")}
    preferred = evaluate(theory, Flags(argument_labelling_mode="preferred"))
    assert sorted(sorted(g.conclusions("in")) for g in preferred) == [["-d"], ["d"]]


def test_the_sub_arguments_are_given():
    (graph,) = evaluate("f1 :=> a.\nr1 : a => b.\n")
    b = next(argument for argument in graph.arguments if argument.conclusion == "b")
    assert graph.argument(b.supports[0]).conclusion == "a"
    assert b.top_rule == "r1"


def test_clauses_are_added_to_the_theory():
    solver = Solver("f1 :=> a.\n")
    assert solver.answer("a").in_ == ["a"]
    solver.add("f2 :=> -a.")
    assert solver.answer("a").und == ["a"]


def test_an_abstract_framework():
    (graph,) = evaluate("graphBuildMode(abstract).\na -> b.\nb -> c.\n")
    assert graph.conclusions("in") == {"a", "c"}
    assert graph.conclusions("out") == {"b"}
