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

case class Offsets(roomOffset : Int, doorOffset : Int) derives ReadWriter

object DynamicSolver {
    def processStep(g : MyGraph, input : StepInput, offset : Offsets) : SolverStepResult =
        println(s"Try: from room #${input.roomFromUid}, via door #${input.doorIdx}, to room labeled ${input.roomToLabel}, room offset = ${offset.roomOffset}, door offset = ${offset.doorOffset}")
        if (offset.roomOffset > g.rooms.length - 1) {
            TargetRoomsExhausted()
        } else if (offset.doorOffset > 5) {
            TargetDoorsExhausted()
        } else {
            val existingLinkedRoom = g.adjacentRoom(g.doors(input.roomFromUid)(input.doorIdx))
            existingLinkedRoom match {
                case None => {
                    val existingCandidateRoomsWithDoors = g.findRoomsByLabelWithFreeDoors(input.roomToLabel)
                    val existingCandidateRooms = existingCandidateRoomsWithDoors.keys.toSeq.sortBy(_.uid)
                    println(s"Rooms already labeled ${input.roomToLabel}: $existingCandidateRooms")
                    if (existingCandidateRooms.length > offset.roomOffset) {
                        val existingCandidateDoors = existingCandidateRoomsWithDoors(existingCandidateRooms(offset.roomOffset))
                        if (existingCandidateDoors.length > offset.doorOffset) {
                            val nextDoor = existingCandidateDoors(offset.doorOffset)
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
                        val freeRoomOffset = offset.roomOffset - existingCandidateRooms.length
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

    def processStepRecursiveEnumerate(graph : MyGraph, currentRoomUid : Int, inputs : Seq[LabelInput], offsets : Seq[Offsets] = Seq()) : SolverStepResult =
        var iteration = 0
        var subRoomOffset = 0
        var subDoorOffset = 0
        while (iteration < 100)
            val subOffsets = Offsets(subRoomOffset, subDoorOffset) +: offsets
            println(f"Step into, iteration $iteration; remaining length is ${inputs.tail.length}; offsets: $subOffsets")
            val subResult = processStepRecursive(graph, inputs, currentRoomUid = currentRoomUid, offsets = subOffsets)
            println(s"Returning from recursive call: $subResult")
            subResult match {
                case StepApplied(_,_) => return subResult
                case TargetRoomsExhausted() => return Contradiction()
                case TargetDoorsExhausted() =>
                    subRoomOffset += 1
                    subDoorOffset = 0
                case Contradiction() =>
                    subDoorOffset += 1
            }
            iteration += 1
        Impossible()

    def processStepRecursive(graph : MyGraph, inputs : Seq[LabelInput], currentRoomUid : Int = 0, offsets : Seq[Offsets] = Seq(Offsets(0,0))) : SolverStepResult =
        if (inputs.length == 0) {
            println("no inputs left")
            StepApplied(graph, DoorVertex(currentRoomUid, 0))
        } else {
            val stepInput = StepInput(currentRoomUid, inputs.head.doorIdx, inputs.head.roomToLabel)
            val stepResult = processStep(graph, stepInput, offsets.head)
            println(s"Returning from single step call: $stepResult")
            stepResult match {
                case StepApplied(newGraph, door) =>
                    processStepRecursiveEnumerate(newGraph, door.roomUid, inputs = inputs.tail, offsets=offsets)
                case _ => stepResult
            }
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
        processStepRecursiveEnumerate(graph, currentRoomUid = srcRoomUid, inputs)

}
