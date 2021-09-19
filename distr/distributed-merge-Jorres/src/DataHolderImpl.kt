import system.DataHolderEnvironment
import java.lang.Math.min

class DataHolderImpl<T : Comparable<T>>(
    private val keys: List<T>,
    private val dataHolderEnvironment: DataHolderEnvironment
) : DataHolder<T> {
    private var checkpoint: Int = 0
    private var firstNext: Int = 0

    override fun checkpoint() {
        checkpoint = firstNext
    }

    override fun rollBack() {
        firstNext = checkpoint
    }

    override fun getBatch(): List<T> {
        val curBegin = firstNext
        var curEnd = curBegin + dataHolderEnvironment.batchSize
        if (curEnd > keys.size) {
            curEnd = keys.size
        }

        if (curBegin > curEnd) {
            return listOf()
        }
        firstNext = curEnd
        val lst = mutableListOf<T>()
        for (tmp in curBegin until curEnd) {
            lst.add(keys[tmp])
        }
        return lst
    }
}