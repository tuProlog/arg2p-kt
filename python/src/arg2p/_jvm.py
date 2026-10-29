"""The JVM running arg2p: started once, at the first use, with the jars of the package, on the Java runtime it
bundles (that of the system, for a package without one)."""

from __future__ import annotations

import glob
import os
import shlex
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


def options() -> list[str]:
    """
    The options the JVM starts with: the stack of ``ARG2P_STACK``, then those of ``ARG2P_JVM_OPTIONS`` (as
    ``-Xmx8g``, for the heap of large graphs), split as a shell splits them (see split).
    """
    return [f"-Xss{STACK}", "-Dslf4j.internal.verbosity=ERROR", *split(os.environ.get("ARG2P_JVM_OPTIONS", ""))]


def split(text: str, windows: bool = sys.platform == "win32") -> list[str]:
    """
    [text] split at blanks as a shell splits it: what is quoted (``"-Dlog=C:/My logs"``) is one, without the quotes. A
    backslash escapes the next character, but on Windows, where it separates the directories of a path.
    """
    lexer = shlex.shlex(text, posix=True)
    lexer.whitespace_split = True
    lexer.commenters = ""
    if windows:
        lexer.escape = ""
    return list(lexer)


def classpath() -> list[str]:
    """
    The jars of arg2p: those of ``ARG2P_CLASSPATH``, then those of the package; a class of the first is the one
    loaded, and the package gives what they lack (the MCP server, say).
    """
    stated = [it for it in os.environ.get("ARG2P_CLASSPATH", "").split(os.pathsep) if it]
    jars = [*stated, *sorted(glob.glob(os.path.join(JARS, "*.jar")))]
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


def java() -> str:
    """
    The java command of the runtime of the library of the JVM (see jvm_path), in a directory above it: java on the path
    if there is none, or no library.
    """
    executable = "java.exe" if sys.platform == "win32" else "java"
    try:
        directory = os.path.dirname(os.path.abspath(jvm_path()))
    except (jpype.JVMNotFoundException, jpype.JVMNotSupportedException):
        return "java"
    for _ in range(4):  # bin/server/jvm.dll, lib/server/libjvm.so, jre/lib/amd64/server/libjvm.so
        directory = os.path.dirname(directory)
        if os.path.isfile(os.path.join(directory, "bin", executable)):
            return os.path.join(directory, "bin", executable)
    return "java"


def start() -> None:
    """Starts the JVM, unless it runs already (started by another library: the jars of arg2p must be on its path)."""
    with _lock:
        if not jpype.isJVMStarted():
            jpype.startJVM(jvm_path(), *options(), classpath=classpath(), convertStrings=True)


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
