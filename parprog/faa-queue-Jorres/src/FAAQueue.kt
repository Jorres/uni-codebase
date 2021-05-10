import kotlinx.atomicfu.*

class FAAQueue<T> {
    private val head: AtomicRef<Segment>
    private val tail: AtomicRef<Segment>

    init {
        val firstNode = Segment()
        head = atomic(firstNode)
        tail = atomic(firstNode)
    }

    fun enqueue(x: T) {
        var tailv = tail.value
        while (true) {
            val myId = tailv.enqIdx.getAndIncrement()
            if (myId >= SEGMENT_SIZE) {
                val newTail = Segment(x)
                if (tailv.next.compareAndSet(null, newTail)) {
                    tail.compareAndSet(tailv, newTail)
                    return
                } else {
                    tailv = tailv.next.value!!
                }
            } else {
                if (tailv.elements[myId].compareAndSet(null, x)) {
                    return
                }
            }
        }
    }

    fun dequeue(): T? {
        while (true) {
            val headv = head.value
            val myId = headv.deqIdx.getAndIncrement()
            if (myId >= SEGMENT_SIZE) {
                if (headv.next.value == null) {
                    return null
                }
                head.compareAndSet(headv, headv.next.value!!)
                continue
            }
            val res = headv.elements[myId].getAndSet(DONE) ?: continue
            return res as T?
        }
    }

    val isEmpty: Boolean get() {
        return true
    }
}

private class Segment {
    val next: AtomicRef<Segment?> = atomic(null)
    val enqIdx = atomic(0) // index for the next enqueue operation
    val deqIdx = atomic(0) // index for the next dequeue operation
    val elements = atomicArrayOfNulls<Any>(SEGMENT_SIZE)

    constructor() // for the first segment creation

    constructor(x: Any?) { // each next new segment should be constructed with an element
        enqIdx.getAndIncrement()
        elements[0].getAndSet(x)
    }
}

private val DONE = Any() // Marker for the "DONE" slot state; to avoid memory leaks
const val SEGMENT_SIZE = 2 // DO NOT CHANGE, IMPORTANT FOR TESTS
