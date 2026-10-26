# Module parallel-solver

The parallel solver of arg2p: a catalog partitions the theory among workers, which build the arguments of their rules
in parallel, sending each argument directly to the workers that build on it, and then find the direct attacks on their
own arguments, each pair checked once. The catalog gathers the graph and labels it as the standard solver does, with
any semantics and graph extension.

The same catalog and workers run on one thread (`LocalCluster`), on the threads of the JVM (`ThreadedCluster`), or on
the threads of Node.js (`JsWorkerCluster`). On a machine the nodes share the terms of their messages; between processes
the messages are written as Prolog text (`MessageCodec`).
