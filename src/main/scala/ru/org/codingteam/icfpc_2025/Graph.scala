package ru.org.codingteam.icfpc_2025

import scala.collection.mutable.ArrayBuffer
import scalax.collection.edges.UnDiEdge
import scalax.collection.edges.UnDiEdgeImplicits
import scalax.collection.immutable.Graph
import scalax.collection.OuterImplicits.anyToNode
import upickle.ReadWriter

abstract class MyVertex()
case class RoomVertex(uid : Int, label : Option[Int]) extends MyVertex derives ReadWriter
case class DoorVertex(roomUid : Int, idx : Int) extends MyVertex derives ReadWriter

class MyGraph private (val graph : Graph[MyVertex, UnDiEdge[MyVertex]],
        val rooms : Seq[RoomVertex],
        val doors : Map[Int,Seq[DoorVertex]]) {

    def addRoom(uid : Int, label : Option[Int] = None) =
        val room = RoomVertex(uid, label)
        val newDoors = (0 to 5).map(i =>
                DoorVertex(uid, i)
            )
        val edges = newDoors.map(door => room ~ door)
        // val roomGraph = Graph(edges)
        new MyGraph(graph.concat(edges),
            room +: rooms,
            doors ++ Map(room.uid -> newDoors)
        )

    def setRoomLabel(uid : Int, newLabel : Option[Int]) =
        val neighbors = graph.get(rooms(uid)).neighbors.map(_.outer)
        val graphWithoutOldRoom = graph.excl(rooms(uid))
        val newRoom = RoomVertex(uid, newLabel)
        val newEdges = neighbors.map(door => newRoom ~ door)
        new MyGraph(graphWithoutOldRoom.concat(newEdges),
            rooms.updated(uid, newRoom),
            doors)

    def connectRooms(uid1 : Int, door1id : Int, uid2 : Int, door2id : Int) =
        var door1 = doors(uid1)(door1id)
        var door2 = doors(uid2)(door2id)
        new MyGraph(
            graph.incl(door1 ~ door2),
            rooms,
            doors
        )

    def otherEnd(edge : graph.GraphInnerEdge, vertex : MyVertex) =
        val innerVertex = graph.get(vertex)
        if (edge.node1 == innerVertex) {
            edge.node2
        } else {
            edge.node1
        }

    def findUnlabeledRooms =
        rooms.filter(r => r.label == None)

    def findRoomsByLabel(label : Int) =
        rooms.filter(r => r.label == Some(label))

    def findFreeDoors(roomUid : Int) =
        doors(roomUid).filter(d => graph.get(d).degree == 1)

    def findFirstFreeDoor(roomUid : Int) : Option[DoorVertex] =
        val freeDoors = findFreeDoors(roomUid)
        if (freeDoors.length == 0)
            None
        else
            Some(freeDoors(0))

    def adjacentDoor(door : DoorVertex) : Option[DoorVertex] =
        val innerVertex = graph.get(door)
        val doorToDoorEdges = innerVertex.edges.flatMap(e =>
                if (e.node1.outer.isInstanceOf[RoomVertex] || e.node2.outer.isInstanceOf[RoomVertex])
                    None
                else
                    Some(e)
            ).toSeq
        if (doorToDoorEdges.length == 0)
            None
        else
            val edge = doorToDoorEdges(0)
            val node1 = edge.node1.outer.asInstanceOf[DoorVertex]
            val node2 = edge.node2.outer.asInstanceOf[DoorVertex]
            if (node1 == door && node2 == door)
                Some(door)
            else if (node1 != door)
                Some(node1)
            else
                Some(node2)

    def adjacentRoom(door : DoorVertex) : Option[RoomVertex] =
        adjacentDoor(door) match {
            case None => None
            case Some(otherDoor) => Some(rooms(otherDoor.roomUid))
        }

    def neighborRooms(roomUid : Int): Set[RoomVertex] =
        val innerVertex = graph.get(rooms(roomUid))
        innerVertex.neighbors.map(_.outer).flatMap(door => adjacentRoom(door.asInstanceOf[DoorVertex])).toSet

    def neighborRoomsByDoor(roomUid : Int): Map[Int, RoomVertex] =
        val innerVertex = graph.get(rooms(roomUid))
        val pairs = innerVertex.neighbors.map(_.outer).flatMap(vertex =>
                val door = vertex.asInstanceOf[DoorVertex]
                adjacentRoom(door) match {
                    case None => Seq()
                    case Some(room) => Seq(door.idx -> room)
                }).toSeq
        Map(pairs*)

    def findAllDoorPassages : Set[(DoorVertex, DoorVertex)] =
      graph.edges.toOuter.filter(e => e.node1.isInstanceOf[DoorVertex] && e.node2.isInstanceOf[DoorVertex])
          .map(e => (e.node1.asInstanceOf[DoorVertex], e.node2.asInstanceOf[DoorVertex]))

    def toSolution: SolutionDefinition =
        val solRooms = rooms.map(_.label.get)
        val connections = findAllDoorPassages.map((door1, door2) =>
                ConnectionDefinition(Door(door1.roomUid, door1.idx), Door(door2.roomUid, door2.idx))
                ).toSeq
        SolutionDefinition(solRooms, 0, connections)

}

object MyGraph {
    def apply() = new MyGraph(Graph(), Seq(), Map.empty)

    def apply(nRooms : Int) =
        val empty = new MyGraph(Graph(), Seq(), Map.empty)
        (0 until nRooms).foldRight(empty)((i, acc) => acc.addRoom(i))
}


