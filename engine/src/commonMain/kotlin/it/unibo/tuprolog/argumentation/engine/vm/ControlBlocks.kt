package it.unibo.tuprolog.argumentation.engine.vm

/**
 * The code every machine of a program runs besides the clauses (see Machine): the end of a run, the control constructs called
 * as goals at run time (their goals in the registers), and the return from an inlined goal. Assembled once for each program:
 * a machine for each solve would leave them behind in its code.
 */
internal class ControlBlocks private constructor(
    /** A `STOP` instruction: the continuation of a run that ends with a solution */
    val stop: Int,
    /** `','(A, B)`, the goals in registers 0 and 1 */
    val conjunction: Int,
    /** `';'(A, B)` (not an if-then-else), the goals in registers 0 and 1 */
    val disjunction: Int,
    /** `(C -> T ; E)`, the goals in registers 0, 1 and 2 */
    val ifThenElse: Int,
    /** `\+ G`, the goal in register 0 */
    val negation: Int,
    /** The return of an inlined goal: the scope of the caller restored from the frame the inlining allocated, the frame deallocated */
    val restoreScope: Int,
) {
    companion object {
        /** The blocks of [program], assembled at the end of its code */
        fun of(program: Program): ControlBlocks {
            val emitter = CodeEmitter(program)

            fun block(assemble: () -> Unit): Int = program.codeTop.also { assemble() }

            // A choice between the goal in register 0 and what follows the else label, the frame of [slots] slots: [then] once the
            // goal succeeds, [otherwise] in its place
            fun choice(
                slots: Int,
                inRegisters: Int,
                level: Boolean,
                opacity: Int,
                then: () -> Unit,
                otherwise: () -> Unit,
            ) {
                program.emit(Op.ALLOCATE, slots)
                for (i in 0 until inRegisters) program.emit(Op.GET_Y_VAR, i, i)
                if (level) program.emit(Op.GET_LEVEL, slots - 1)
                program.emit(Op.TRY_ELSE)
                val elseLabel = program.emit(0)
                program.emit(Op.CALL_TERM, 0, opacity)
                then()
                emitter.placeLabel(elseLabel)
                program.emit(Op.TRUST_ME)
                otherwise()
            }

            // The goal in frame slot [slot] executed in place of the block, its frame deallocated
            fun executeSlot(slot: Int) {
                program.emit(Op.PUT_Y_VAL, slot, 0)
                program.emit(Op.DEALLOCATE)
                program.emit(Op.EXECUTE_TERM, 0, Op.TRANSPARENT)
            }

            return ControlBlocks(
                stop = block { program.emit(Op.STOP) },
                conjunction =
                    block {
                        program.emit(Op.ALLOCATE, 2)
                        program.emit(Op.GET_Y_VAR, 0, 0)
                        program.emit(Op.GET_Y_VAR, 1, 1)
                        program.emit(Op.CALL_TERM, 0, Op.TRANSPARENT)
                        executeSlot(1)
                    },
                disjunction =
                    block {
                        choice(2, 2, level = false, Op.TRANSPARENT, then = { program.emit(Op.RETURN) }, otherwise = { executeSlot(1) })
                    },
                ifThenElse =
                    block {
                        choice(4, 3, level = true, Op.OPAQUE, then = {
                            program.emit(Op.CUT_Y, 3)
                            executeSlot(1)
                        }, otherwise = { executeSlot(2) })
                    },
                negation =
                    block {
                        choice(2, 1, level = true, Op.OPAQUE, then = {
                            program.emit(Op.CUT_Y, 1)
                            program.emit(Op.FAIL)
                        }, otherwise = { program.emit(Op.RETURN) })
                    },
                restoreScope = block { program.emit(Op.RESTORE_SCOPE_RETURN) },
            )
        }
    }
}
