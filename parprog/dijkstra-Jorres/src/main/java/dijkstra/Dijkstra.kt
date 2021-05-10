package dijkstra

import kotlinx.atomicfu.atomic
import java.lang.Integer.max
import java.util.*
import java.util.concurrent.Phaser
import java.util.concurrent.locks.Lock
import java.util.concurrent.locks.ReentrantLock
import kotlin.Comparator
import kotlin.concurrent.thread
import kotlin.random.Random.Default.nextInt

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> {
    o1, o2 -> o1!!.distance.compareTo(o2!!.distance)
}

val active = atomic(0)
val nonEmptyQueues = atomic(0)

class MultiQPQ(threads: Int) {
    private val _threads = max(2, threads)
    private val _queues = ArrayList<PriorityQueue<Node>>()

    private val _locks = ArrayList<Lock>()

    init {
        for (i in 0 until _threads) {
            _queues.add(PriorityQueue(_threads, NODE_DISTANCE_COMPARATOR))
            _locks.add(ReentrantLock())
        }
    }

    fun add(x: Node) {
        while (true) {
            val queueNum = nextInt(0, _threads - 1)
            if (_locks[queueNum].tryLock()) {
                if (_queues[queueNum].isEmpty()) {
                    nonEmptyQueues.incrementAndGet()
                }

                _queues[queueNum].add(x)
                _locks[queueNum].unlock()
                return
            }
        }
    }

    fun poll(): Node? {
        val queueFirst = nextInt(0, _threads - 1)
        var queueSecond = queueFirst
        while (queueSecond != queueFirst) {
            queueSecond = nextInt(0, _threads - 1)
        }

        if (_locks[queueFirst].tryLock()) {
            if (_locks[queueSecond].tryLock()) {
                val left = _queues[queueFirst].peek()
                val right = _queues[queueSecond].peek()
                var ans: Node? = null
                if (left != null || right != null) {
                    val best: Int
                    if (left == null)
                        best = queueSecond
                    else if (right == null)
                        best = queueFirst
                    else if (NODE_DISTANCE_COMPARATOR.compare(left, right) >= 0) {
                        best = queueSecond
                    } else {
                        best = queueFirst
                    }
                    if (_queues[best].size == 1) {
                        nonEmptyQueues.decrementAndGet()
                    }
                    ans = _queues[best].poll()
                }

                _locks[queueSecond].unlock()
                _locks[queueFirst].unlock()

                if (ans != null) {
                    return ans
                }
            } else {
                _locks[queueFirst].unlock()
            }
        }
        return null;
    }
}

fun shortestPathParallel(start: Node) {
    val threads = Runtime.getRuntime().availableProcessors()
    val q = MultiQPQ(threads)
    start.distance = 0

    q.add(start)

    active.incrementAndGet()

    val onFinish = Phaser(threads + 1)
    repeat(threads) {
        thread {
            while (active.value > 0) {
                val cur: Node = q.poll() ?: continue
                val curDist = cur.distance
                for (e in cur.outgoingEdges) {
                    val myDist = curDist + e.weight
                    while (true) {
                        val toDist = e.to.distance
                        if (toDist > myDist) {
                            if (e.to.casDistance(toDist, myDist)) {
                                active.incrementAndGet()
                                q.add(e.to)
                                break
                            }
                        } else {
                            break
                        }
                    }
                }
                active.decrementAndGet()
            }
            onFinish.arrive()
        }
    }

    onFinish.arriveAndAwaitAdvance()
}
