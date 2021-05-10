import java.lang.ThreadLocal

/**
 * @author Tarasov Egor
 */
class Solution : AtomicCounter {
    val _cur: ThreadLocal<Node>
    val globalRoot = Node(0)
    init {
        _cur = ThreadLocal.withInitial({globalRoot})
    }

    override fun getAndAdd(x: Int): Int {
        while (true) {
            val last = _cur.get()
            val updated = last.myValue + x
            val cp = Node(updated)
            val lNode = last.next.decide(cp)
            _cur.set(lNode)
            if (lNode === cp) {
                return updated - x
            }
        }
    }

    class Node(value: Int) {
        val myValue = value
        val next: Consensus<Node> = Consensus()
    }
}
