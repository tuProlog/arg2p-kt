package it.unibo.tuprolog.argumentation.federation

import it.unibo.tuprolog.argumentation.parallel.message.TextReader
import it.unibo.tuprolog.argumentation.parallel.message.TextWriter

/** The statistics of a federation as text, for a site on another thread than its parallel solver */
internal object StatsCodec {
    fun encode(stats: FederationStats): String =
        TextWriter()
            .text(stats.site)
            .list(stats.sites) { text(it.id).text(it.name).texts(listOfNotNull(it.workers?.toString())) }
            .toString()

    fun decodeFederation(text: String): FederationStats =
        TextReader(text).run {
            val site = text()
            FederationStats(site, list { SiteStats(text(), text(), texts().singleOrNull()?.toInt()) })
        }
}
