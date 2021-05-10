import kotlinx.atomicfu.*
import java.lang.IllegalArgumentException

class DynamicArrayImpl<E> : DynamicArray<E> {
    private val core = atomic(Core<E>(INITIAL_CAPACITY))
    private val sz = atomic(0)

    override fun get(index: Int): E {
        var curCore = core.value
        if (index >= sz.value) {
            throw IllegalArgumentException()
        }
        while (true) {
            if (index >= curCore.capacity.value) {
                curCore = curCore.next.value!!
                continue
            }

            val observed = curCore.array[index].value
            if (observed is FixedValue) {
                val nextCore = curCore.next.value
                nextCore!!.array[index].compareAndSet(null, OkValue(observed.inside))
                curCore = nextCore
            } else {
                return observed!!.inside
            }
        }
    }

    override fun put(index: Int, element: E) {
        var curCore = core.value
        if (index >= sz.value) {
            throw IllegalArgumentException()
        }

        while (true) {
            if (index >= curCore.capacity.value) {
                curCore = curCore.next.value!!
                continue
            }

            val observed = curCore.array[index].value
            if (observed is FixedValue) {
                curCore = curCore.next.value!!
            } else {
                if (curCore.array[index].compareAndSet(observed, OkValue(element))) {
                    return
                }
            }
        }
    }

    private fun trySetNewHead(new: Core<E>) {
        var i = 0
        while (true) {
            val oldOne = core.value
            if (new.capacity.value > oldOne.capacity.value) {
                if (core.compareAndSet(oldOne, new)) {
                    return
                }
            } else {
                return
            }
        }
    }
    
    //   0 1 2 3 4 
    // [ | | | | ] n     
    // [ x x | | x x ] n  
    // [ . . x x . . y y y y ] e
    // [ . . . . . . . . . . . . . . . . ]
    // [ . . . . x . . . . . . . . . . . . . . . . . . . . . . ] 


    override fun pushBack(element: E) {
        while (true) {
            val old = core.value
            val oldSz = sz.value
            val oldCp = old.capacity.value

            if (oldSz < oldCp) {
                val oldTarget = old.array[oldSz].value
                if (oldTarget == null) {
                    if (old.array[oldSz].compareAndSet(oldTarget, OkValue(element))) {
                        sz.compareAndSet(oldSz, oldSz + 1)
                        return
                    }  else {
                        sz.compareAndSet(oldSz, oldSz + 1)
                    }
                }
            } else {
                val new = Core<E>(oldCp * 2)
                if (old.next.compareAndSet(null, new)) {
                    var i = 0
                    while (i < oldCp) {
                        val oldElemVal = old.array[i].value
                        if (old.array[i].compareAndSet(oldElemVal, FixedValue(oldElemVal!!.inside))) {
                            // sleep
                            new.array[i].compareAndSet(null, OkValue(oldElemVal.inside))
                            i++
                        }
                    }
                    trySetNewHead(new)
                }
            }
        }
    }

    override val size: Int get() {
        return sz.value
    }
}

interface Wrapper<E> {
    val inside: E
}

private class FixedValue<E>(value: E) : Wrapper<E> {
    override val inside = value
}

private class OkValue<E>(value: E) : Wrapper<E> {
    override val inside = value
}

private class Core<E>(
    capacity: Int,
) {
    val array = atomicArrayOfNulls<Wrapper<E>>(capacity)
    val next: AtomicRef<Core<E>?> = atomic(null)
    val capacity = atomic(capacity)
}

private const val INITIAL_CAPACITY = 1 // DO NOT CHANGE ME
