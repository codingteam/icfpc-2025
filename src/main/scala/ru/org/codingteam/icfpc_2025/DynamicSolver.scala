package ru.org.codingteam.icfpc_2025

object DynamicSolver {
    def processStep(g : MyGraph, roomFromUid : Int, doorIdx : Int, roomToLabel : Int) : Option[MyGraph] =
        val existingLinkedRoom = g.adjacentRoom(g.doors(roomFromUid)(doorIdx))
        existingLinkedRoom match {
            case None => {
                val existingCandidateRooms = g.findRoomsByLabel(roomToLabel)
                val existingCandidateDoors = existingCandidateRooms.flatMap(room => g.findFirstFreeDoor(room.uid))
                if (existingCandidateDoors.length > 0) {
                    val nextDoor = existingCandidateDoors(0)
                    Some(
                        g.connectRooms(roomFromUid, doorIdx, nextDoor.roomUid, nextDoor.idx)
                    )
                } else {
                    val freeRooms = g.findUnlabeledRooms
                    if (freeRooms.length > 0) {
                        val freeRoom = freeRooms(0)
                        val labeledGraph = g.setRoomLabel(freeRoom.uid, Some(roomToLabel))
                        Some(
                            labeledGraph.connectRooms(roomFromUid, doorIdx, freeRoom.uid, 0)
                        )
                    } else {
                        None
                    }
                }
            }
            case Some(linkedRoom) => {
                if (linkedRoom.label == Some(roomToLabel)) {
                    Some(g)
                } else {
                    None
                }
            }
        }
}
