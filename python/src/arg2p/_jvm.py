"""The JVM running arg2p: started once, at the first use, with the jars of the package."""

from __future__ import annotations

import glob
import os
import threading
from typing import Any, Callable, TypeVar

import jpype

T = TypeVar("T")

_lock = threading.Lock()

#: The jars of the package, copied there by ``./gradlew :parallel-solver:pythonJars``
JARS = os.path.join(os.path.dirname(__file__), "jars")

#: The stack of the threads of the JVM: resolution goes deep (the tests of arg2p run with the same)
STACK = os.environ.get("ARG2P_STACK", "512m")


def classpath() -> list[str]:
    """The jars of arg2p: those of ``ARG2P_CLASSPATH`` if set, those of the package otherwise."""
    stated = os.environ.get("ARG2P_CLASSPATH")
    if stated:
        return stated.split(os.pathsep)
    jars = sorted(glob.glob(os.path.join(JARS, "*.jar")))
    if not jars:
        raise RuntimeError(
            "No jar of arg2p in " + JARS + ": copy them with ./gradlew :parallel-solver:pythonJars, "
            "or set ARG2P_CLASSPATH"
        )
    return jars


def start() -> None:
    """Starts the JVM, unless it runs already (started by another library: the jars of arg2p must be on its path)."""
    with _lock:
        if not jpype.isJVMStarted():
            jpype.startJVM(f"-Xss{STACK}", "-Dslf4j.internal.verbosity=ERROR", classpath=classpath(), convertStrings=True)


def jclass(name: str) -> Any:
    """The Java class [name], the JVM started."""
    start()
    return jpype.JClass(name)


@jpype.JImplements("java.lang.Runnable", deferred=True)
class _Task:
    def __init__(self, function: Callable[[], Any]) -> None:
        self.function = function
        self.value: Any = None
        self.error: BaseException | None = None

    @jpype.JOverride
    def run(self) -> None:
        try:
            self.value = self.function()
        except BaseException as error:  # noqa: BLE001 - raised again by the caller
            self.error = error


def run(function: Callable[[], T]) -> T:
    """The result of [function], run on a thread of the JVM, whose stack resolution needs (see STACK)."""
    start()
    task = _Task(function)
    thread = jpype.JClass("java.lang.Thread")(task, "arg2p")
    thread.setDaemon(True)
    thread.start()
    thread.join()
    if task.error is not None:
        raise task.error
    return task.value


def companion(name: str) -> Any:
    """The companion object of the Kotlin class [name] (JPype gives the class of the companion for its name)."""
    return jclass(name).class_.getField("Companion").get(None)


def items(sequence: Any, limit: int | None = None, each: Callable[[Any], Any] | None = None) -> list[Any]:
    """
    The items of [sequence], a Kotlin Sequence (at most [limit]), each given to [each] as the sequence reaches it
    (before the next is computed): its iterator may be of a class JPype does not reach, the list Kotlin makes is not.
    """
    sequences = jclass("kotlin.sequences.SequencesKt")
    if limit is not None:
        sequence = sequences.take(sequence, limit)
    if each is not None:
        sequence = sequences.map(sequence, _Function(each))
    return list(sequences.toList(sequence))


@jpype.JImplements("kotlin.jvm.functions.Function1", deferred=True)
class _Function:
    def __init__(self, function: Callable[[Any], Any]) -> None:
        self.function = function

    @jpype.JOverride
    def invoke(self, value: Any) -> Any:
        return self.function(value)
