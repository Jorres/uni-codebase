package dijkstra.messages

sealed class Message

data class MessageWithDist(val dist: Long) : Message()

object MessageNewChild : Message()
object MessageRemoveChild : Message()
