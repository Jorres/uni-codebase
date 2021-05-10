package dijkstra

import dijkstra.messages.*
import dijkstra.system.environment.Environment

class ProcessImpl(private val environment: Environment) : Process {
    val ACKNOWLEDGE: Long = -1
    enum class TreeStatus {RED, GREEN}

    var parentId = -1

    var myChildren = 0
    var messageBal = 0
    var myDistance: Long? = null
    var myStatus = TreeStatus.GREEN

    fun checkForRemoval() {
        check(myStatus == TreeStatus.RED)
        if (myChildren == 0 && messageBal == 0) {
            myStatus = TreeStatus.GREEN
            if (parentId != -1) {
                environment.send(parentId, MessageRemoveChild)
            } else {
                environment.finishExecution()
            }
        }
    }

    override fun onMessage(srcId: Int, message: Message) {
        if (message is MessageNewChild) {
            myChildren++
            return
        }
        if (message is MessageRemoveChild) {
            myChildren--
            checkForRemoval()
            return
        }
        check(message is MessageWithDist)

        val dist = message.dist
        if (dist == ACKNOWLEDGE) {
            messageBal--
            checkForRemoval()
        } else {
            // TODO sure in message order????
            if (myStatus == TreeStatus.GREEN) {
                environment.send(srcId, MessageNewChild)
                myStatus = TreeStatus.RED
                parentId = srcId
            }

            environment.send(srcId, MessageWithDist(ACKNOWLEDGE))
            if (myDistance == null || myDistance!! > dist) {
                myDistance = dist
                environment.neighbours.forEach{(id, dist) ->
                    messageBal++
                    environment.send(id, MessageWithDist(myDistance!! + dist))
                }
            }
            checkForRemoval()
        }
    }

    override fun getDistance() = myDistance

    override fun startComputation() {
        myDistance = 0
        myStatus = TreeStatus.RED

        environment.neighbours.forEach{(id, dist) ->
            messageBal++
            environment.send(id, MessageWithDist(dist))
        }

        checkForRemoval()
    }
}