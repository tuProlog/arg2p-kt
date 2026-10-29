"""
The labels of the statements of a theory, from the command line: python -m arg2p theory.pl [-q query] ...

``arg2p mcp`` runs the MCP server of arg2p instead, for AI assistants: over the standard input and output, on the Java
runtime of the package. ``arg2p page`` writes the recorded page of a query solved on a theory, which shows the run
(see recorded.py).
"""

from __future__ import annotations

import argparse
import os
import subprocess
import sys
from typing import Any, Optional

from . import _jvm, recorded
from .arg2p import Arg2p, Arg2pError
from .flags import Flags, Profile, Semantics
from .values import text


class _Failure(Exception):
    """What the command line fails with, as it tells it: arg2p: <the failure>, and the exit status 1"""


def main(argv: Optional[list[str]] = None) -> int:
    arguments = sys.argv[1:] if argv is None else argv
    try:
        if arguments[:1] == ["mcp"]:
            return mcp(arguments[1:])
        if arguments[:1] == ["page"]:
            return page(arguments[1:])
        return evaluate(arguments)
    except (_Failure, Arg2pError) as error:
        print(f"arg2p: {error}", file=sys.stderr)
        return 1


def mcp(argv: list[str]) -> int:
    """Runs the MCP server of arg2p until its client closes the standard input (a theory named mcp: ./mcp)"""
    argparse.ArgumentParser(prog="arg2p mcp", description="Runs the MCP server of arg2p, for AI assistants.").parse_args(argv)
    try:
        from ._version import MCP_SERVER
    except ImportError:  # a package built without Gradle, or before the server was written into it
        raise _Failure("no MCP server in the package: write it with ./gradlew :python:pythonVersion") from None
    return _run([_jvm.java(), *_jvm.options(), "-cp", os.pathsep.join(_jvm.classpath()), MCP_SERVER, "--mcp"])


def _run(command: list[str]) -> int:
    """
    The exit status of [command], run in place of this process on POSIX: the signals sent to arg2p mcp reach it, and it
    leaves no process behind. Windows has no such thing: there it runs as a child, killed with this process however it
    ends (see _killed_with_this_process), and stopped when this process is interrupted.
    """
    sys.stdout.flush()
    sys.stderr.flush()
    if os.name != "nt":
        os.execvp(command[0], command)  # does not return
    child = subprocess.Popen(command)
    _killed_with_this_process(child)
    try:
        return child.wait()
    except KeyboardInterrupt:
        child.terminate()
        child.wait()
        return 130


def _killed_with_this_process(child: subprocess.Popen[Any]) -> None:
    """
    [child] put in a job of Windows that kills its processes once its handle is closed: by this process, as it ends,
    however it does (killed too). Left as it is if Windows refuses it.
    """
    import ctypes
    from ctypes import wintypes

    class BasicLimits(ctypes.Structure):
        _fields_ = [
            ("PerProcessUserTimeLimit", ctypes.c_int64),
            ("PerJobUserTimeLimit", ctypes.c_int64),
            ("LimitFlags", wintypes.DWORD),
            ("MinimumWorkingSetSize", ctypes.c_size_t),
            ("MaximumWorkingSetSize", ctypes.c_size_t),
            ("ActiveProcessLimit", wintypes.DWORD),
            ("Affinity", ctypes.c_size_t),
            ("PriorityClass", wintypes.DWORD),
            ("SchedulingClass", wintypes.DWORD),
        ]

    class IoCounters(ctypes.Structure):
        _fields_ = [(name, ctypes.c_uint64) for name in ("Reads", "Writes", "Others", "Read", "Written", "Other")]

    class Limits(ctypes.Structure):  # JOBOBJECT_EXTENDED_LIMIT_INFORMATION
        _fields_ = [
            ("BasicLimitInformation", BasicLimits),
            ("IoInfo", IoCounters),
            ("ProcessMemoryLimit", ctypes.c_size_t),
            ("JobMemoryLimit", ctypes.c_size_t),
            ("PeakProcessMemoryUsed", ctypes.c_size_t),
            ("PeakJobMemoryUsed", ctypes.c_size_t),
        ]

    kill_on_job_close, extended_limit_information = 0x2000, 9
    set_quota_and_terminate = 0x0100 | 0x0001
    kernel32 = ctypes.WinDLL("kernel32", use_last_error=True)  # type: ignore[attr-defined]
    kernel32.CreateJobObjectW.restype = wintypes.HANDLE
    kernel32.CreateJobObjectW.argtypes = [ctypes.c_void_p, wintypes.LPCWSTR]
    kernel32.SetInformationJobObject.argtypes = [wintypes.HANDLE, ctypes.c_int, ctypes.c_void_p, wintypes.DWORD]
    kernel32.OpenProcess.restype = wintypes.HANDLE
    kernel32.OpenProcess.argtypes = [wintypes.DWORD, wintypes.BOOL, wintypes.DWORD]
    kernel32.AssignProcessToJobObject.argtypes = [wintypes.HANDLE, wintypes.HANDLE]
    kernel32.CloseHandle.argtypes = [wintypes.HANDLE]
    # the job: its handle is never closed but by Windows, as this process ends
    job = kernel32.CreateJobObjectW(None, None)
    if not job:
        return
    limits = Limits()
    limits.BasicLimitInformation.LimitFlags = kill_on_job_close
    if not kernel32.SetInformationJobObject(job, extended_limit_information, ctypes.byref(limits), ctypes.sizeof(limits)):
        kernel32.CloseHandle(job)
        return
    process = kernel32.OpenProcess(set_quota_and_terminate, False, child.pid)
    if process:
        kernel32.AssignProcessToJobObject(job, process)
        kernel32.CloseHandle(process)


class _PageHelp(argparse.HelpFormatter):
    """The help of arg2p page, each option with its default as the recorded page has it (the JVM started for it)"""

    def _get_help_string(self, action: argparse.Action) -> Optional[str]:
        try:
            default = recorded.defaults().get(action.dest)
        except Exception:  # noqa: BLE001 - no JVM to ask: the help without the defaults
            default = None
        if default is None or action.help is None:
            return action.help
        written = str(default).replace("\n", "\\n").replace("%", "%%")
        return f"{action.help} (default: {written})"


def page(argv: list[str]) -> int:
    """
    Writes the recorded page of a query solved on a theory (a theory named page: ./page), and prints where: in the
    file of -o, or arg2p-<the name of the theory>.html (arg2p-page.html for the standard input)
    """
    parser = argparse.ArgumentParser(
        prog="arg2p page",
        description="Writes an HTML page showing a query solved on a theory: its solutions, their argument graphs, the "
        "flags in effect and the theory. The page solves nothing: for another query, or another theory, make another.",
        formatter_class=_PageHelp,
    )
    parser.add_argument("theory", help="the file of the theory (- for the standard input)")
    parser.add_argument("-q", "--query", help="the query")
    parser.add_argument("-f", "--flags", help="the flags, as Prolog facts, \\n between them a new line")
    parser.add_argument("-o", "--output", help="the file of the page (default: arg2p-<theory>.html)")
    parser.add_argument("--max-solutions", type=int, help="how many solutions at most")
    parser.add_argument("--timeout", type=int, help="how long to solve, in seconds")
    arguments = parser.parse_args(argv)
    theory = _read(arguments.theory)
    name = "page" if arguments.theory == "-" else os.path.splitext(os.path.basename(arguments.theory))[0]
    output = arguments.output or f"arg2p-{name}.html"
    flags = None if arguments.flags is None else arguments.flags.replace("\\n", "\n")
    _write(output, recorded.page(theory, arguments.query, flags, arguments.max_solutions, arguments.timeout))
    print(os.path.abspath(output))
    return 0


def evaluate(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(
        prog="arg2p",
        description="Evaluates a theory of arg2p.",
        epilog="arg2p mcp runs the MCP server of arg2p instead, for AI assistants (see arg2p mcp --help); arg2p page "
        "writes an HTML page showing a query solved on a theory (see arg2p page --help). A theory in a file named mcp "
        "or page is ./mcp, ./page.",
    )
    parser.add_argument("theory", help="the file of the theory (- for the standard input)")
    parser.add_argument("-q", "--query", action="append", help="a statement to evaluate (all of them by default)")
    parser.add_argument("-p", "--profile", choices=[it.value for it in Profile], help="the profile of the theory")
    parser.add_argument("-s", "--semantics", choices=[it.value for it in Semantics], help="the semantics")
    arguments = parser.parse_args(argv)
    theory = _read(arguments.theory)
    flags = Flags.defaults()
    if arguments.profile:
        flags.graph_build_mode(Profile(arguments.profile))
    if arguments.semantics:
        flags.argument_labelling_mode(Semantics(arguments.semantics))
    arg2p = Arg2p.of(theory, flags)
    if arguments.query:
        for query in arguments.query:
            answer = arg2p.answer(query)
            print(
                f"{query}: in {text(answer.in_statements)}, out {text(answer.out_statements)}, "
                f"und {text(answer.und_statements)}"
            )
    else:
        for index, graph in enumerate(arg2p.graphs()):
            if index:
                print()
            for label in ("in", "out", "und"):
                print(f"{label}: {', '.join(sorted(graph.conclusions(label)))}")
    return 0


def _read(path: str) -> str:
    """The text of the file [path] (- the standard input): a _Failure if it cannot be read"""
    if path == "-":
        return sys.stdin.read()
    try:
        with open(path, encoding="utf-8") as file:
            return file.read()
    except OSError as error:
        raise _Failure(f"cannot read {path}: {error.strerror or error}") from None
    except UnicodeDecodeError:
        raise _Failure(f"cannot read {path}: not UTF-8 text") from None


def _write(path: str, written: str) -> None:
    """[written] in the file [path]: a _Failure if it cannot be written"""
    try:
        with open(path, "w", encoding="utf-8") as file:
            file.write(written)
    except OSError as error:
        raise _Failure(f"cannot write {path}: {error.strerror or error}") from None


if __name__ == "__main__":
    sys.exit(main())
