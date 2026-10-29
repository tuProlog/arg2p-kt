package it.unibo.tuprolog.argumentation.lsp

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.google.gson.JsonObject
import com.google.gson.JsonPrimitive

// The parameters of the requests the servers are sent, as JSON gives them: each field read as what it has to be, an
// InvalidParams telling which one is not otherwise.

/** A request whose parameters are not those its method takes: the error invalid params of JSON-RPC */
internal class InvalidParams(
    message: String,
) : IllegalArgumentException(message)

/** The JSON of the servers: the null fields kept, as the pages tell null from a missing field */
internal val json: Gson = GsonBuilder().serializeNulls().disableHtmlEscaping().create()

/** The text [name]: null if it is missing */
internal fun JsonObject.text(name: String): String? = field(name, "a text") { it.isString }?.asString

/** The integer [name]: null if it is missing */
internal fun JsonObject.integer(name: String): Long? =
    field(name, "an integer") { it.isNumber && it.asBigDecimal.stripTrailingZeros().scale() <= 0 }?.asLong

/** The boolean [name]: null if it is missing */
internal fun JsonObject.flag(name: String): Boolean? = field(name, "a boolean") { it.isBoolean }?.asBoolean

/** The object [name]: null if it is missing */
internal fun JsonObject.obj(name: String): JsonObject? =
    get(name)?.takeUnless { it.isJsonNull }?.let {
        it as? JsonObject
            ?: invalid(name, "an object")
    }

/** The text [name], which is required */
internal fun JsonObject.requiredText(name: String): String = text(name) ?: throw InvalidParams("$name is missing")

/** The integer [name], which is required */
internal fun JsonObject.requiredInteger(name: String): Long = integer(name) ?: throw InvalidParams("$name is missing")

// The primitive [name] when it is what [kind] tells
private fun JsonObject.field(
    name: String,
    kind: String,
    valid: (JsonPrimitive) -> Boolean,
): JsonPrimitive? =
    get(name)?.takeUnless { it.isJsonNull }?.let { value ->
        (value as? JsonPrimitive)?.takeIf(valid) ?: invalid(name, kind)
    }

private fun invalid(
    name: String,
    kind: String,
): Nothing = throw InvalidParams("$name is not $kind")
