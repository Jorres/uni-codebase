import java.util.concurrent.atomic.*

class Solution(private val env: Environment) : Lock<Solution.Node> {
    private val tail = AtomicReference<Node>(null)

    override fun lock(): Node {
        val my = Node()
        val prev = tail.getAndSet(my)
        if (prev != null) {
            prev.next.getAndSet(my)
            while (my.locked.get()) {
                env.park()
            }
        }
        return my
    }

    override fun unlock(node: Node) {
        if (node.next.get() == null) {
            if (tail.compareAndSet(node, null)) {
                return
            } else {
                while (node.next.get() == null) {
                    continue
                }
            }
        }

        node.next.value.locked.set(false)
        env.unpark(node.next.value.thread)
    }

    class Node {
        val thread = Thread.currentThread()
        val locked = AtomicReference<Boolean>(true)
        val next = AtomicReference<Node>(null)
    }
}