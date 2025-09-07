package ru.org.codingteam.icfpc_2025

import upickle.ReadWriter

abstract class SolverStepResult() {
    def get : MyGraph =
        throw Exception("Result graph was not provided")
}
case class StepApplied(graph : MyGraph, nextDoor : DoorVertex) extends SolverStepResult {
    override def get : MyGraph = graph
}
case class TargetDoorsExhausted() extends SolverStepResult derives ReadWriter
case class TargetRoomsExhausted() extends SolverStepResult derives ReadWriter
case class Contradiction() extends SolverStepResult derives ReadWriter
case class Rollback() extends SolverStepResult derives ReadWriter
case class Impossible() extends SolverStepResult derives ReadWriter

case class LabelInput(roomFromLabel : Int, doorIdx : Int, roomToLabel : Int) derives ReadWriter
case class StepInput(roomFromUid : Int, doorIdx : Int, roomToLabel : Int) derives ReadWriter

object DynamicSolver {
    def processStep(g : MyGraph, input : StepInput, targetRoomOffset : Int = 0, targetDoorOffset : Int = 0) : SolverStepResult =
        println(s"Try: from room #${input.roomFromUid}, via door #${input.doorIdx}, to room labeled ${input.roomToLabel}, room offset = $targetRoomOffset, door offset = $targetDoorOffset")
        if (targetRoomOffset > g.rooms.length - 1) {
            TargetRoomsExhausted()
        } else if (targetDoorOffset > 5) {
            TargetDoorsExhausted()
        } else {
            val existingLinkedRoom = g.adjacentRoom(g.doors(input.roomFromUid)(input.doorIdx))
            existingLinkedRoom match {
                case None => {
                    val existingCandidateRooms = g.findRoomsByLabel(input.roomToLabel)
                    println(s"Rooms already labeled ${input.roomToLabel}: $existingCandidateRooms")
                    if (existingCandidateRooms.length > targetRoomOffset) {
                        val existingCandidateDoors = g.findFreeDoors(existingCandidateRooms(targetRoomOffset).uid)
                        if (existingCandidateDoors.length > targetDoorOffset) {
                            val nextDoor = existingCandidateDoors(targetDoorOffset)
                            println(s"Connect room #${input.roomFromUid} door ${input.doorIdx} to room ${nextDoor.roomUid}, door ${nextDoor.idx}")
                            StepApplied(
                                g.connectRooms(input.roomFromUid, input.doorIdx, nextDoor.roomUid, nextDoor.idx),
                                nextDoor
                            )
                        } else {
                            TargetDoorsExhausted()
                        }
                    } else {
                        val freeRooms = g.findUnlabeledRooms
                        val freeRoomOffset = targetRoomOffset - existingCandidateRooms.length
                        if (freeRooms.length > freeRoomOffset) {
                            val freeRoom = freeRooms(freeRoomOffset)
                            val labeledGraph = g.setRoomLabel(freeRoom.uid, Some(input.roomToLabel))
                            println(s"Connect room #${input.roomFromUid} door ${input.doorIdx} to room ${freeRoom.uid}, door ${0}")
                            StepApplied(
                                labeledGraph.connectRooms(input.roomFromUid, input.doorIdx, freeRoom.uid, 0),
                                DoorVertex(freeRoom.uid, 0)
                            )
                        } else {
                            TargetRoomsExhausted()
                        }
                    }
                }
                case Some(linkedRoom) => {
                    println(s"Room #${input.roomFromUid} door ${input.doorIdx} was already connected to room #${linkedRoom.uid} (label ${linkedRoom.label})")
                    if (linkedRoom.label == Some(input.roomToLabel)) {
                        val nextDoor = g.adjacentDoor(g.doors(input.roomFromUid)(input.doorIdx)).get
                        StepApplied(g, nextDoor)
                    } else {
                        Contradiction()
                    }
                }
            }
        }

    def processStepEnumerating(graph : MyGraph, inputs : Seq[LabelInput], currentRoomUid : Int = 0, roomOffset : Int = 0, doorOffset : Int = 0) : SolverStepResult =
        if (inputs.length == 0) {
            println("no inputs left")
            StepApplied(graph, DoorVertex(currentRoomUid, 0))
        } else {
            val stepInput = StepInput(currentRoomUid, inputs.head.doorIdx, inputs.head.roomToLabel)
            val stepResult = processStep(graph, stepInput, roomOffset, doorOffset)
            stepResult match {
                case StepApplied(newGraph, door) =>
                    println(f"Step into; remaining length is ${inputs.tail.length}")
                    val subResult = processStepEnumerating(newGraph, inputs.tail, currentRoomUid = door.roomUid)
                    println(s"Returning from recursive call: $subResult")
                    subResult match {
                        case StepApplied(_,_) => return subResult
                        case _ =>
                            println(s"Do rollback; remaining length is ${inputs.length}")
                            return processStepEnumerating(graph, inputs, currentRoomUid, roomOffset = roomOffset, doorOffset = doorOffset+1)
                    }
                case TargetDoorsExhausted() =>
                    println("Doors exhausted")
                    return processStepEnumerating(graph, inputs, 0, roomOffset+1, 0)
                case TargetRoomsExhausted() =>
                    println("Rooms exhausted")
                    return TargetRoomsExhausted()
                case Contradiction() =>
                    println("Contradiction")
                    //return processStepEnumerating(graph, inputs, currentRoomUid, roomOffset = roomOffset, doorOffset = doorOffset+1)
                    return Rollback()
                case Rollback() =>
                    println("Rollback")
                    return processStepEnumerating(graph, inputs, currentRoomUid, roomOffset = roomOffset, doorOffset = doorOffset+1)
            }
            Impossible()
        }

    def makeInput(plan : Seq[Int], roomLabels : Seq[Int]) : Seq[LabelInput] =
        var prevLabel = roomLabels.head
        var result = Seq[LabelInput]()
        for (i <- plan.indices)
            val doorIdx = plan(i)
            val roomLabel = roomLabels.tail(i)
            val item = LabelInput(prevLabel, doorIdx, roomLabel)
            prevLabel = roomLabel
            result = result :+ item
        result

    def processPlanAndRooms(graph : MyGraph, srcRoomUid : Int, plan : Seq[Int], roomLabels : Seq[Int]) : SolverStepResult =
        val inputs = makeInput(plan, roomLabels)
        processStepEnumerating(graph, inputs, currentRoomUid = srcRoomUid)

}
