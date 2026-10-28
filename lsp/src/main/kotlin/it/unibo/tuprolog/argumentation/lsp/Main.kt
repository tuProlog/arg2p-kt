package it.unibo.tuprolog.argumentation.lsp

import org.eclipse.lsp4j.jsonrpc.Launcher
import java.io.InputStream
import java.io.OutputStream
import kotlin.system.exitProcess

/** The server of [server], speaking the protocol of language servers on [input] and [output] */
fun launcherOf(
    server: Arg2pLanguageServer,
    input: InputStream,
    output: OutputStream,
): Launcher<Arg2pClient> =
    Launcher
        .Builder<Arg2pClient>()
        .setLocalService(server)
        .setRemoteInterface(Arg2pClient::class.java)
        .setInput(input)
        .setOutput(output)
        .create()
        .also { server.connect(it.remoteProxy) }

/**
 * The language server of arg2p, on the standard input and output (as editors start it). Anything else printing on the
 * standard output would break the protocol: it prints on the standard error instead.
 */
fun main() {
    val protocol = System.out
    System.setOut(System.err)
    val launcher = launcherOf(Arg2pLanguageServer { exitProcess(0) }, System.`in`, protocol)
    launcher.startListening().get()
}
