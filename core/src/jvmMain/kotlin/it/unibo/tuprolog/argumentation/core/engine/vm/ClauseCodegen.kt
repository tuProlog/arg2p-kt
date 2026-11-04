package it.unibo.tuprolog.argumentation.core.engine.vm

import java.io.ByteArrayOutputStream
import java.io.DataOutputStream
import java.lang.invoke.MethodHandles

/**
 * The instructions of a clause as a method of the JVM, its operands constants: no dispatch nor operand to fetch, and the
 * JIT of the JVM inlines the helpers of the [Machine] into it. Control leaves it at calls: the method gives the address
 * to go on at, and each address the clause may be resumed at (after a call, an alternative) is an entry of the method.
 */
internal actual object ClauseCodegen {
    private val lookup = MethodHandles.lookup()
    private const val MACHINE = "it/unibo/tuprolog/argumentation/core/engine/vm/Machine"
    private const val COMPILED = "it/unibo/tuprolog/argumentation/core/engine/vm/CompiledCode"
    private const val GENERATED = "it/unibo/tuprolog/argumentation/core/engine/vm/Generated"

    actual fun generate(
        code: IntArray,
        start: Int,
    ): GeneratedClause? {
        val entries = analyse(code, start) ?: return null
        val bytes = classBytes(code, start, entries.first, entries.second) ?: return null
        val unit =
            lookup
                .defineHiddenClass(bytes, true)
                .lookupClass()
                .getDeclaredConstructor()
                .newInstance() as CompiledCode
        return GeneratedClause(unit, entries.second)
    }

    // The length of the instruction at [at], 0 if it is not one this knows
    private fun length(
        code: IntArray,
        at: Int,
    ): Int =
        when (code[at]) {
            Op.GET_X_VAR, Op.GET_Y_VAR, Op.GET_X_VAL, Op.GET_Y_VAL, Op.GET_CONST, Op.GET_STR -> 3
            Op.GET_LIST -> 2
            Op.GET_LIST_VV -> 6
            Op.GET_LIST2 -> 8
            Op.GET_STR2 -> 9
            Op.UNIFY_X_VAR, Op.UNIFY_Y_VAR -> 3
            Op.UNIFY_X_VAL, Op.UNIFY_Y_VAL, Op.UNIFY_CONST, Op.UNIFY_VOID -> 2
            Op.PUT_X_VAR, Op.PUT_Y_VAR -> 4
            Op.PUT_X_VAL, Op.PUT_Y_VAL, Op.PUT_CONST, Op.PUT_STR -> 3
            Op.PUT_LIST -> 2
            Op.SET_X_VAR, Op.SET_Y_VAR -> 3
            Op.SET_X_VAL, Op.SET_Y_VAL, Op.SET_CONST, Op.SET_VOID -> 2
            Op.ALLOCATE -> 2
            Op.DEALLOCATE -> 1
            Op.CALL, Op.EXECUTE, Op.DEALLOCATE_EXECUTE -> 3
            Op.RETURN, Op.PROCEED, Op.CUT, Op.TRUST_ME, Op.FAIL, Op.RESTORE_SCOPE -> 1
            Op.GET_LEVEL, Op.CUT_Y, Op.CUT_LOCAL, Op.TRY_ELSE, Op.JUMP -> 2
            Op.CALL_TERM, Op.EXECUTE_TERM, Op.INIT_Y -> 3
            Op.ARITH_IS -> 4 + code[at + 3]
            Op.ARITH_CMP -> code[at + 2].let { nA -> 4 + nA + code[at + 3 + nA] }
            Op.TYPE_X, Op.TYPE_Y -> 3
            Op.IDENTICAL, Op.IDENTICAL_C -> 4
            Op.ARG -> 5
            Op.FUNCTOR -> 6
            else -> 0
        }

    private fun terminal(op: Int) =
        op == Op.EXECUTE ||
            op == Op.DEALLOCATE_EXECUTE ||
            op == Op.RETURN ||
            op == Op.PROCEED ||
            op == Op.FAIL ||
            op == Op.EXECUTE_TERM ||
            op == Op.JUMP

    // The end of the clause from [start] and the addresses it is entered at (the start first); null if it has something
    // this does not know
    private fun analyse(
        code: IntArray,
        start: Int,
    ): Pair<Int, IntArray>? {
        val entries = LinkedHashSet<Int>()
        entries += start
        var at = start
        var furthest = start
        while (true) {
            val n = length(code, at)
            if (n == 0) return null
            val op = code[at]
            val next = at + n
            when (op) {
                Op.CALL, Op.CALL_TERM, Op.ARITH_IS, Op.ARITH_CMP, Op.ARG, Op.FUNCTOR -> entries += next
                Op.TRY_ELSE -> {
                    entries += code[at + 1]
                    furthest = maxOf(furthest, code[at + 1])
                }
                Op.JUMP -> furthest = maxOf(furthest, code[at + 1])
                else -> Unit
            }
            at = next
            if (terminal(op) && at > furthest) return at to entries.toIntArray()
        }
    }

    private class Writer {
        val pool = ByteArrayOutputStream()
        private val poolData = DataOutputStream(pool)
        private val known = HashMap<String, Int>()
        var count = 1

        private fun entry(
            key: String,
            write: DataOutputStream.() -> Unit,
        ): Int =
            known.getOrPut(key) {
                poolData.write()
                count++
            }

        fun utf8(text: String): Int =
            entry("u$text") {
                writeByte(1)
                writeUTF(text)
            }

        fun type(name: String): Int {
            val n = utf8(name)
            return entry("c$name") {
                writeByte(7)
                writeShort(n)
            }
        }

        fun integer(value: Int): Int =
            entry("i$value") {
                writeByte(3)
                writeInt(value)
            }

        fun method(
            owner: String,
            name: String,
            descriptor: String,
            interfaceMethod: Boolean = false,
        ): Int {
            val c = type(owner)
            val n = utf8(name)
            val d = utf8(descriptor)
            val nameAndType =
                entry("n$name$descriptor") {
                    writeByte(12)
                    writeShort(n)
                    writeShort(d)
                }
            return entry("m$owner.$name$descriptor") {
                writeByte(if (interfaceMethod) 11 else 10)
                writeShort(c)
                writeShort(nameAndType)
            }
        }
    }

    // The bytes of a method, with labels: jumps patched when the method is done
    private class Body(
        private val writer: Writer,
    ) {
        val out = ByteArrayOutputStream()
        private val labels = HashMap<Int, Int>()
        private val patches = ArrayList<Triple<Int, Int, Int>>()

        val position: Int get() = out.size()

        fun label(address: Int) {
            labels[address] = position
        }

        fun byte(value: Int) = out.write(value)

        private fun short(value: Int) {
            out.write(value shr 8)
            out.write(value)
        }

        fun int(value: Int) {
            when (value) {
                in -1..5 -> byte(0x03 + value)
                in -128..127 -> {
                    byte(0x10)
                    byte(value)
                }
                in -32768..32767 -> {
                    byte(0x11)
                    short(value)
                }
                else -> {
                    byte(0x13)
                    short(writer.integer(value))
                }
            }
        }

        fun machine() = byte(0x2B)

        /** A helper of the machine on constants, leaving what it answers */
        fun helper(
            name: String,
            result: String,
            vararg arguments: Int,
        ) {
            machine()
            for (argument in arguments) int(argument)
            byte(0xB6)
            short(writer.method(MACHINE, name, "(" + "I".repeat(arguments.size) + ")" + result))
        }

        /** A helper answering whether to go on: failing otherwise */
        fun check(
            name: String,
            vararg arguments: Int,
        ) {
            helper(name, "Z", *arguments)
            jump(0x99, FAILURE)
        }

        fun jump(
            opcode: Int,
            address: Int,
        ) {
            patches += Triple(position, position + 1, address)
            byte(opcode)
            short(0)
        }

        /** A helper answering the address to go on at, [next] to go on with the following instruction */
        fun continuing(
            name: String,
            next: Int,
            vararg arguments: Int,
        ) {
            helper(name, "I", *arguments, next)
            byte(0x3D) // (istore_2: the local of the entry, no longer needed)
            byte(0x1C)
            int(next)
            jump(0x9F, next)
            byte(0x1C)
            byte(0xAC)
        }

        fun returnFrom(
            name: String,
            vararg arguments: Int,
        ) {
            helper(name, "I", *arguments)
            byte(0xAC)
        }

        fun switchOn(addresses: IntArray) {
            byte(0x1C)
            val from = position
            byte(0xAA)
            while (position % 4 != 0) byte(0)
            val default = position
            repeat(4) { byte(0) }
            for (shift in intArrayOf(24, 16, 8, 0)) byte(0 shr shift)
            val high = addresses.size - 1
            for (shift in intArrayOf(24, 16, 8, 0)) byte(high shr shift)
            val tables = position
            repeat(addresses.size * 4) { byte(0) }
            wide += Wide(from, default, addresses[0])
            for ((i, address) in addresses.withIndex()) wide += Wide(from, tables + i * 4, address)
        }

        private class Wide(
            val from: Int,
            val at: Int,
            val address: Int,
        )

        private val wide = ArrayList<Wide>()

        fun bytes(): ByteArray? {
            val bytes = out.toByteArray()
            if (bytes.size > 60000) return null
            for ((from, at, address) in patches) {
                val offset = labels[address]!! - from
                bytes[at] = (offset shr 8).toByte()
                bytes[at + 1] = offset.toByte()
            }
            for (w in wide) {
                val offset = labels[w.address]!! - w.from
                bytes[w.at] = (offset shr 24).toByte()
                bytes[w.at + 1] = (offset shr 16).toByte()
                bytes[w.at + 2] = (offset shr 8).toByte()
                bytes[w.at + 3] = offset.toByte()
            }
            return bytes
        }

        companion object {
            const val FAILURE = -1
        }
    }

    private fun Body.sub(
        kind: Int,
        a: Int,
        b: Int,
    ) {
        when (kind) {
            Op.U_X_VAR -> helper("jUnifyXVar", "V", a, b)
            Op.U_Y_VAR -> helper("jUnifyYVar", "V", a, b)
            Op.U_X_VAL -> check("jUnifyXVal", a)
            Op.U_Y_VAL -> check("jUnifyYVal", a)
            Op.U_CONST -> check("jUnifyConst", a)
            else -> helper("jUnifyVoids", "V", 1)
        }
    }

    private fun Body.instruction(
        code: IntArray,
        at: Int,
        next: Int,
    ) {
        val a = if (at + 1 < code.size) code[at + 1] else 0
        val b = if (at + 2 < code.size) code[at + 2] else 0
        when (code[at]) {
            Op.GET_X_VAR -> helper("jMove", "V", a, b)
            Op.GET_Y_VAR -> helper("jGetYVar", "V", a, b)
            Op.GET_X_VAL -> check("jGetXVal", a, b)
            Op.GET_Y_VAL -> check("jGetYVal", a, b)
            Op.GET_CONST -> check("jGetConst", a, b)
            Op.GET_STR -> check("jGetStructure", a, b)
            Op.GET_LIST -> check("jGetList", a)
            Op.GET_LIST_VV -> check("jGetListVV", a, b, code[at + 3], code[at + 4], code[at + 5])
            Op.GET_LIST2 -> {
                check("jGetList", a)
                sub(b, code[at + 3], code[at + 4])
                sub(code[at + 5], code[at + 6], code[at + 7])
            }
            Op.GET_STR2 -> {
                check("jGetStructure", a, b)
                sub(code[at + 3], code[at + 4], code[at + 5])
                sub(code[at + 6], code[at + 7], code[at + 8])
            }
            Op.UNIFY_X_VAR -> helper("jUnifyXVar", "V", a, b)
            Op.UNIFY_Y_VAR -> helper("jUnifyYVar", "V", a, b)
            Op.UNIFY_X_VAL -> check("jUnifyXVal", a)
            Op.UNIFY_Y_VAL -> check("jUnifyYVal", a)
            Op.UNIFY_CONST -> check("jUnifyConst", a)
            Op.UNIFY_VOID -> helper("jUnifyVoids", "V", a)
            Op.PUT_X_VAR -> helper("jPutXVar", "V", a, b, code[at + 3])
            Op.PUT_Y_VAR -> helper("jPutYVar", "V", a, b, code[at + 3])
            Op.PUT_X_VAL -> helper("jMove", "V", b, a)
            Op.PUT_Y_VAL -> helper("jPutYVal", "V", a, b)
            Op.PUT_CONST -> helper("jPutConst", "V", a, b)
            Op.PUT_STR -> helper("jPutStructure", "V", a, b)
            Op.PUT_LIST -> helper("jPutList", "V", a)
            Op.SET_X_VAR -> helper("jSetXVar", "V", a, b)
            Op.SET_Y_VAR -> helper("jSetYVar", "V", a, b)
            Op.SET_X_VAL -> helper("jSetXVal", "V", a)
            Op.SET_Y_VAL -> helper("jSetYVal", "V", a)
            Op.SET_CONST -> helper("jSetConst", "V", a)
            Op.SET_VOID -> helper("jSetVoids", "V", a)
            Op.ALLOCATE -> helper("jAllocate", "V", a)
            Op.DEALLOCATE -> helper("jDeallocate", "V")
            Op.CALL -> continuing("jCall", next, a, b)
            Op.EXECUTE -> returnFrom("jExecute", a, b)
            Op.DEALLOCATE_EXECUTE -> {
                helper("jDeallocate", "V")
                returnFrom("jExecute", a, b)
            }
            Op.RETURN -> {
                helper("jDeallocate", "V")
                returnFrom("jContinuation")
            }
            Op.PROCEED -> returnFrom("jContinuation")
            Op.CUT -> helper("jCut", "V")
            Op.GET_LEVEL -> helper("jGetLevel", "V", a)
            Op.CUT_Y -> helper("jCutY", "V", a)
            Op.CUT_LOCAL -> helper("jCutLocal", "V", a)
            Op.TRY_ELSE -> helper("jTryElse", "V", a)
            Op.TRUST_ME -> Unit
            Op.JUMP -> jump(0xA7, a)
            Op.FAIL -> jump(0xA7, Body.FAILURE)
            Op.CALL_TERM -> continuing("jCallTerm", next, a, b)
            Op.EXECUTE_TERM -> returnFrom("jExecuteTerm", a, b)
            Op.INIT_Y -> helper("jInitY", "V", a, b)
            Op.RESTORE_SCOPE -> helper("jRestoreScope", "V")
            Op.ARITH_IS -> continuing("jArithIs", next, at)
            Op.ARITH_CMP -> continuing("jArithCompare", next, at)
            Op.TYPE_X -> check("jTypeX", a, b)
            Op.TYPE_Y -> check("jTypeY", a, b)
            Op.IDENTICAL -> check("jIdentical", a, b, code[at + 3])
            Op.IDENTICAL_C -> check("jIdenticalConstant", a, b, code[at + 3])
            Op.ARG -> continuing("jArg", next, at)
            Op.FUNCTOR -> continuing("jFunctor", next, at)
            else -> throw IllegalStateException("not generated: ${code[at]}")
        }
    }

    private fun classBytes(
        code: IntArray,
        start: Int,
        end: Int,
        entries: IntArray,
    ): ByteArray? {
        val writer = Writer()
        val body = Body(writer)
        body.switchOn(entries)
        var at = start
        while (at < end) {
            val n = length(code, at)
            body.label(at)
            body.instruction(code, at, at + n)
            at += n
        }
        body.label(end)
        // (a clause ends with a terminal instruction: what follows is reached by no path but the verifier's)
        body.label(Body.FAILURE)
        body.int(-1)
        body.byte(0xAC)
        val method = body.bytes() ?: return null

        val thisClass = writer.type(GENERATED)
        val superClass = writer.type("java/lang/Object")
        val compiled = writer.type(COMPILED)
        val init = writer.utf8("<init>")
        val initDescriptor = writer.utf8("()V")
        val objectInit = writer.method("java/lang/Object", "<init>", "()V")
        val runName = writer.utf8("run")
        val runDescriptor = writer.utf8("(L$MACHINE;I)I")
        val codeAttribute = writer.utf8("Code")

        val out = ByteArrayOutputStream()
        val data = DataOutputStream(out)
        data.writeInt(0xCAFEBABE.toInt())
        data.writeShort(0)
        data.writeShort(50)
        data.writeShort(writer.count)
        writer.pool.writeTo(data)
        data.writeShort(0x0031)
        data.writeShort(thisClass)
        data.writeShort(superClass)
        data.writeShort(1)
        data.writeShort(compiled)
        data.writeShort(0)
        data.writeShort(2)
        // the constructor
        data.writeShort(0x0001)
        data.writeShort(init)
        data.writeShort(initDescriptor)
        data.writeShort(1)
        data.writeShort(codeAttribute)
        data.writeInt(12 + 5)
        data.writeShort(1)
        data.writeShort(1)
        data.writeInt(5)
        data.writeByte(0x2A)
        data.writeByte(0xB7)
        data.writeShort(objectInit)
        data.writeByte(0xB1)
        data.writeShort(0)
        data.writeShort(0)
        // the method
        data.writeShort(0x0001)
        data.writeShort(runName)
        data.writeShort(runDescriptor)
        data.writeShort(1)
        data.writeShort(codeAttribute)
        data.writeInt(12 + method.size)
        data.writeShort(16)
        data.writeShort(3)
        data.writeInt(method.size)
        data.write(method)
        data.writeShort(0)
        data.writeShort(0)
        data.writeShort(0)
        return out.toByteArray()
    }
}
