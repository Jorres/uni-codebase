package mutex

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Egor Tarasov
 */
class ProcessImpl(private val env: Environment) : Process {
    enum class ForkStatus {DIRTY, CLEAN, NOT_MINE}
    enum class MsgType {ASK_FORK, HAND_FORK}
    enum class ProcessStatus {REQ, IDLE}

    private val iHaveTheFork: Array<ForkStatus> = Array(env.nProcesses) {ForkStatus.DIRTY}
    private val isWaitingForMe: Array<Boolean> = Array(env.nProcesses) {false}
    private val myProcessId = env.processId - 1
    private var currentStatus = ProcessStatus.IDLE

    init {
        for (i in 0 until env.nProcesses) {
            if (i > myProcessId) {
                iHaveTheFork[i] = ForkStatus.NOT_MINE
            }
        }
    }

    private fun shortSend(type: MsgType, to: Int) {
        check(to >= 0 && to < env.nProcesses)
        env.send(to + 1) {
            writeEnum(type)
        }
    }

    override fun onMessage(srcId: Int, message: Message) {
        val _srcId = srcId - 1
        message.parse {
            when (readEnum<MsgType>()) {
                MsgType.ASK_FORK -> {
                    check(!isWaitingForMe[_srcId])
                    check(iHaveTheFork[_srcId] != ForkStatus.NOT_MINE)
                    if (iHaveTheFork[_srcId] == ForkStatus.DIRTY) {
                        iHaveTheFork[_srcId] = ForkStatus.NOT_MINE
                        shortSend(MsgType.HAND_FORK, _srcId)
                        if (currentStatus == ProcessStatus.REQ) {
                            shortSend(MsgType.ASK_FORK, _srcId)
                        } else {
                            return
                        }
                    } else {
                        check(iHaveTheFork[_srcId] == ForkStatus.CLEAN)
                        check(_srcId != myProcessId)
                        check(currentStatus == ProcessStatus.REQ)
                        isWaitingForMe[_srcId] = true
                    }
                }
                MsgType.HAND_FORK -> {
                    check(iHaveTheFork[_srcId] == ForkStatus.NOT_MINE)
                    iHaveTheFork[_srcId] = ForkStatus.CLEAN
                    checkCsEnter()
                }
            }
        }
    }

    fun checkCsEnter(): Boolean {
        var canEnter = true
        iHaveTheFork.forEachIndexed { i, st ->
            if (i != myProcessId && st == ForkStatus.NOT_MINE) {
                canEnter = false
            }
        }
        if (canEnter) {
            for (i in 0 until env.nProcesses) {
                iHaveTheFork[i] = ForkStatus.CLEAN
            }
            env.locked()
        }
        return canEnter
    }

    override fun onLockRequest() {
        currentStatus = ProcessStatus.REQ
        if (!checkCsEnter()) {
            iHaveTheFork.forEachIndexed{i, st ->
                if (i != myProcessId && st == ForkStatus.NOT_MINE) {
                    shortSend(MsgType.ASK_FORK, i)
                }
            }
        }
    }

    override fun onUnlockRequest() {
        env.unlocked()
        currentStatus = ProcessStatus.IDLE
        isWaitingForMe.forEachIndexed{i, waits ->
            if (waits) {
                iHaveTheFork[i] = ForkStatus.NOT_MINE
                isWaitingForMe[i] = false
                shortSend(MsgType.HAND_FORK, i)
            } else {
                iHaveTheFork[i] = ForkStatus.DIRTY
            }
        }
        check(isWaitingForMe.all{elem -> !elem })
        check(iHaveTheFork.all{elem -> elem != ForkStatus.CLEAN })
    }
}