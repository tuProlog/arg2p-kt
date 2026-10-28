package it.unibo.tuprolog.argumentation.parallel.cluster

import it.unibo.tuprolog.argumentation.Arg2pException
import it.unibo.tuprolog.argumentation.parallel.message.EvaluationResponse

/** [response], unless it is an error, raised */
internal fun answered(response: EvaluationResponse): EvaluationResponse = response.error?.let { throw Arg2pException(it) } ?: response
