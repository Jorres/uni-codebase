class ConsistentHashImpl<K> : ConsistentHash<K> {
    private var nodes: List<Pair<Int, Shard>> = listOf()

    override fun getShardByKey(key: K): Shard {
        val keyHash = key.hashCode()
        if (keyHash > nodes[nodes.size - 1].first) {
            return nodes[0].second
        }

        var L = -1
        var R = nodes.size - 1
        while (R - L > 1) {
            val M = (L + R) / 2
            val nodeHash = nodes[M].first
            if (keyHash <= nodeHash) {
                R = M
            } else {
                L = M
            }
        }

        return nodes[R].second
    }

    override fun addShard(newShard: Shard, vnodeHashes: Set<Int>): Map<Shard, Set<HashRange>> {
        val newNode = vnodeHashes.map { Pair(it, newShard) }
        mergeWithExisting(newNode)
        return representChange(newShard)
    }

    override fun removeShard(shard: Shard): Map<Shard, Set<HashRange>> {
        val ans = representChange(shard)
        nodes = nodes.filter { it.second != shard }
        return ans
    }

    private fun representChange(shard: Shard): MutableMap<Shard, MutableSet<HashRange>> {
        val ans: MutableMap<Shard, MutableSet<HashRange>> = mutableMapOf()
        var lastNotMe = -1
        var startPos = -1
        for (i in nodes.indices) {
            val (mePos, meShard) = nodes[i]
            if (meShard != shard) {
                lastNotMe = mePos
                startPos = i
                break
            }
        }
        if (startPos == -1) {
            return ans
        }

        for (i in startPos + 1..startPos + nodes.size + 1) {
            val (mePos, meShard) = nodes[i % nodes.size]
            val (nextPos, nextShard) = nodes[(i + 1) % nodes.size]

            if (meShard == shard) {
                if (nextShard != shard) {
                    ans.putIfAbsent(nextShard, mutableSetOf())
                    ans[nextShard]!!.add(HashRange( lastNotMe + 1, mePos ))
                }
            } else {
                lastNotMe = mePos
            }
        }
        return ans
    }

    private fun mergeWithExisting(newNode: List<Pair<Int, Shard>>) {
        nodes = nodes + newNode
        nodes = nodes.sortedBy { it.first }
    }
}