import kotlinx.atomicfu.AtomicRef
import kotlinx.atomicfu.atomic
import kotlin.coroutines.Continuation
import kotlin.coroutines.resume
import kotlin.coroutines.suspendCoroutine

class SynchronousQueueMS<E> : SynchronousQueue<E> {
    private val SEND = 0
    private val RECEIVE = 1

    private val RETRY = -123
    private val OK = 3

    private val dummy = Node<E>(SEND)
    private val head = atomic(dummy)
    private val tail: AtomicRef<Node<E>?> = atomic(dummy)

    override suspend fun send(element: E) {
        while (true) {
            val curHead = head.value
            val curTail = tail.value
            if (curHead == curTail || curTail!!.type.value == SEND) {
                val res = suspendCoroutine<Any> sc@{ cont ->
                    val new = Node(SEND, cont, element)
                    if (!curTail.next.compareAndSet(null, new)) {
                        helpMoveTail(curTail)
                        cont.resume(RETRY)
                    } else {
                        helpMoveTail(curTail)
                    }
                }
                if (res != RETRY) {
                    return
                }
            } else {
                if (head.compareAndSet(curHead, curHead.next.value!!)) {
                    curHead.next.value!!.action!!.resume(element!!)
                    return
                }
            }
        }
    }

    override suspend fun receive(): E {
        while (true) {
            val curHead = head.value
            val curTail = tail.value

            if (curHead == curTail || curTail!!.type.value == RECEIVE) {
                val res = suspendCoroutine<Any> sc@{ cont ->
                    val new = Node<E>(RECEIVE, cont)
                    if (!curTail.next.compareAndSet(null, new)) {
                        helpMoveTail(curTail)
                        cont.resume(RETRY)
                    } else {
                        helpMoveTail(curTail)
                    }
                }
                if (res != RETRY) {
                    return res as E
                }
            } else {
                val nextNode = curHead.next.value!!
                if (head.compareAndSet(curHead, nextNode)) {
                    nextNode.action!!.resume(OK)
                    return nextNode.elem!!
                }
            }
        }
    }

    private fun helpMoveTail(curTail: Node<E>) {
        tail.compareAndSet(curTail, curTail.next.value)
    }

    class Node<E>(type: Int, action: Continuation<Any>? = null, elem: E? = null) {
        val next: AtomicRef<Node<E>?> = atomic(null)
        val action = action
        val elem = elem
        val type = atomic(type)
    }
}
