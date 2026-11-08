# Module federation

Parallel solvers federated, on any machine and engine: each `Federation` makes its solver a `Site`, whose
peers are hosts of its node (see `ConsumerHost` in the module parallel), and which runs consumers of their graphs on its
threads. All a site knows of a graph are the frames between its consumers and its store.

One site listens, the others connect to its URL, presenting its code if it requires one (`Pairing`). A new medium is a `Link`:
`WebSocketLinks`, `MemoryLinks`, or `ExternalLink` for one a page provides. `FederationStats` tells what a federation is.

In JavaScript the site runs on the thread of the node of its parallel solver: the script of its Web Workers calls
`startWebWorker(nodeService)`.
