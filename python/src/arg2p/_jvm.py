"""The JVM running arg2p: started once, at the first use, with the jars of the package, on the Java runtime it
bundles (that of the system, for a package without one)."""

from __future__ import annotations

import glob
import os
import sys
import threading
from typing import Any, Callable, TypeVar

import jpype

T = TypeVar("T")

_lock = threading.Lock()

#: The jars of the package, copied there by ``./gradlew :python:pythonJars``
JARS = os.path.join(os.path.dirname(__file__), "jars")

#: The Java runtime the package bundles, made there by ``./gradlew :python:pythonRuntime``
RUNTIME = os.path.join(os.path.dirname(__file__), "jre")

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
            "No jar of arg2p in " + JARS + ": copy them with ./gradlew :python:pythonJars, "
            "or set ARG2P_CLASSPATH"
        )
    return jars


def jvm_path() -> str:
    """
    The library of the JVM: that of ``ARG2P_JVM`` if set, of the runtime the package bundles if it does, of the system
    otherwise (JAVA_HOME, or where JPype finds one).
    """
    stated = os.environ.get("ARG2P_JVM")
    if stated:
        return stated
    library = {"win32": ("bin", "server", "jvm.dll"), "darwin": ("lib", "server", "libjvm.dylib")}.get(
        sys.platform, ("lib", "server", "libjvm.so")
    )
    bundled = os.path.join(RUNTIME, *library)
    return bundled if os.path.exists(bundled) else jpype.getDefaultJVMPath()


def start() -> None:
    """Starts the JVM, unless it runs already (started by another library: the jars of arg2p must be on its path)."""
    with _lock:
        if not jpype.isJVMStarted():
            jpype.startJVM(
                jvm_path(), f"-Xss{STACK}", "-Dslf4j.internal.verbosity=ERROR", classpath=classpath(), convertStrings=True
            )


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


@jpype.JImplements("java.util.concurrent.ThreadFactory", deferred=True)
class _Threads:
    @jpype.JOverride
    def newThread(self, runnable: Any) -> Any:
        # daemons, not to hold the JVM when Python exits; their stack is the one of -Xss (see STACK)
        thread = jpype.JClass("java.lang.Thread")(runnable, _THREAD)
        thread.setDaemon(True)
        return thread


_THREAD = "arg2p"
_pool: Any = None


def _threads() -> Any:
    """The threads running arg2p, made as calls need them and kept for the next ones (a minute idle)."""
    global _pool
    with _lock:
        if _pool is None:
            _pool = jpype.JClass("java.util.concurrent.Executors").newCachedThreadPool(_Threads())
    return _pool


def run(function: Callable[[], T]) -> T:
    """The result of [function], run on a thread of the JVM, whose stack resolution needs (see STACK)."""
    start()
    if str(jpype.JClass("java.lang.Thread").currentThread().getName()) == _THREAD:
        return function()  # already on one
    task = _Task(function)
    _threads().submit(task).get()
    if task.error is not None:
        raise task.error
    return task.value


@jpype.JImplements("kotlin.jvm.functions.Function1", deferred=True)
class _Function:
    def __init__(self, function: Callable[[Any], Any]) -> None:
        self.function = function

    @jpype.JOverride
    def invoke(self, value: Any) -> Any:
        return self.function(value)


def function1(function: Callable[[Any], Any]) -> Any:
    """[function] as a Kotlin function of one argument returning Unit."""
    unit = jclass("kotlin.Unit").INSTANCE

    def invoke(value: Any) -> Any:
        function(value)
        return unit

    return _Function(invoke)


def jarray(name: str, values: list[Any]) -> Any:
    """A Java array of the class [name], of [values]."""
    return jpype.JArray(jclass(name))(values)
