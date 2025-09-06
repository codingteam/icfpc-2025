package ru.org.codingteam.icfpc_2025

import scala.collection.mutable.ArrayBuffer
import scalax.collection.edges.UnDiEdge
import scalax.collection.edges.UnDiEdgeImplicits
import scalax.collection.immutable.Graph
import scalax.collection.OuterImplicits.anyToNode
import upickle.ReadWriter

abstract class MyVertex()
case class RoomVertex(uid : Int, label : Int) extends MyVertex derives ReadWriter
case class DoorVertex(roomUid : Int, idx : Int) extends MyVertex derives ReadWriter

class MyGraph private (val graph : Graph[MyVertex, UnDiEdge[MyVertex]],
        val rooms : Seq[RoomVertex],
        val doors : Map[Int,Seq[DoorVertex]]) {

    def addRoom(uid : Int, label : Int) =
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

    def setRoomLabel(uid : Int, newLabel : Int) =
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

    def findRoomsByLabel(label : Int) =
        rooms.filter(r => r.label == label)

    def findFreeDoors(roomUid : Int) =
        doors(roomUid).filter(d => graph.get(d).degree == 1)

    def adjacentDoor(door : DoorVertex) : Option[DoorVertex] =
        val innerVertex = graph.get(door)
        val res = (innerVertex.neighbors.map(_.outer) - rooms(door.roomUid)).toSeq
        if (res.length == 0)
            None
        else
            Some(res(0).asInstanceOf[DoorVertex])

    def adjacentRoom(door : DoorVertex) : Option[RoomVertex] =
        adjacentDoor(door) match {
            case None => None
            case Some(otherDoor) => Some(rooms(otherDoor.roomUid))
        }

}

object MyGraph {
    def apply() = new MyGraph(Graph(), Seq(), Map.empty)

    def apply(nRooms : Int) =
        val empty = new MyGraph(Graph(), Seq(), Map.empty)
        (0 until nRooms).foldRight(empty)((i, acc) => acc.addRoom(i, 0))
}


