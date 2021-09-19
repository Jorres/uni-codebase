import system.MergerEnvironment

class MergerImpl<T : Comparable<T>>(
    private val mergerEnvironment: MergerEnvironment<T>,
    prevStepBatches: Map<Int, List<T>>?
) : Merger<T> {
    private val finished: MutableList<Boolean> =
        MutableList(mergerEnvironment.dataHoldersCount) {false}
    private val lastPosOf: MutableList<Int> =
        MutableList(mergerEnvironment.dataHoldersCount) {0}
    private val myCurBatches: MutableMap<Int, List<T>>

    init {
        myCurBatches = mutableMapOf()
        if (prevStepBatches == null) {
            for (i in 0 until mergerEnvironment.dataHoldersCount) {
                val lst = mergerEnvironment.requestBatch(i)
                if (lst.isEmpty()) {
                    finished[i] = true
                } else {
                    myCurBatches[i] = lst
                }
            }
        } else {
            for ((key, value) in prevStepBatches) {
                myCurBatches[key] = value
            }

            for (i in 0 until mergerEnvironment.dataHoldersCount) {
                if (!myCurBatches.containsKey(i)) {
                    finished[i] = true
                } else {
                    check(prevStepBatches[i]!!.isNotEmpty())
                }
            }
        }
    }

    private fun getLast(proc: Int): T {
        return myCurBatches[proc]!![lastPosOf[proc]]
    }

    private fun tryUpdateProc(proc: Int) {
        if (!finished[proc] && lastPosOf[proc] >= myCurBatches[proc]!!.size) {
            // println("Request to $proc")
            val lst = mergerEnvironment.requestBatch(proc)
            if (lst.isEmpty()) {
                finished[proc] = true
            } else {
                myCurBatches[proc] = lst
                lastPosOf[proc] = 0
            }
        }
    }

    override fun mergeStep(): T? {
        var bestProc: Int = -1
        for (i in 0 until mergerEnvironment.dataHoldersCount) {
            if (!finished[i]) {
                // tryUpdateProc(i)
                // if (finished[i]) {
                //     continue
                // }

                // if (bestProc != -1) {
                //     val a = getLast(bestProc)
                //     val b = getLast(i)
                //     if ((a as Int) < 15 && (b as Int) < 15) {
                //         println(getLast(bestProc).toString() + " " + getLast(i).toString())
                //     }
                // }
                if (bestProc == -1 || getLast(bestProc) > getLast(i)) {
                    bestProc = i
                }
            }
        }
        if (bestProc == -1) {
            return null
        }
        // refactor into always keeping first available => it will be easier
        // to do getRemainingBatches
        val result = getLast(bestProc)
        lastPosOf[bestProc]++
        tryUpdateProc(bestProc)
        return result
    }

    override fun getRemainingBatches(): Map<Int, List<T>> {
        val result = mutableMapOf<Int, List<T>>()
        for (i in 0 until mergerEnvironment.dataHoldersCount) {
            if (!finished[i]) {
                val lst = myCurBatches[i]

                val ans = mutableListOf<T>()
                for (j in lastPosOf[i] until lst!!.size) {
                    ans.add(lst[j])
                }

                check(ans.isNotEmpty())
                result[i] = ans
            }
        }
        return result
    }
}