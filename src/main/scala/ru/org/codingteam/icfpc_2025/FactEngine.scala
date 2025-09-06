package ru.org.codingteam.icfpc_2025

import ru.org.codingteam.icfpc_2025.Fact.HasRoute

import scala.util.boundary

private case class DoorDef(roomLabel: Int, doorIndex: Int)

private case class Route(doors: Seq[Int], roomLabels: Seq[Int]):
    if doors.isEmpty || roomLabels.isEmpty then throw new Exception("Please do not construct an empty route.")

    def head: DoorDef = DoorDef(roomLabels.head, doors.head)
    def tail: Route = Route(doors.tail, roomLabels.tail)
    def tails: Iterator[Route] = doors.zip(roomLabels).tails.map { Route(_) }

    def compatibleWith(other: Route): Boolean =
        val steps1 = doors.zip(roomLabels)
        val steps2 = other.doors.zip(other.roomLabels)

        boundary {
            for (((door1, label1), (door2, label2)) <- steps1.zip(steps2))
                if door1 != door2 then boundary.break(true) // different turn, this is 100% compat
                if label1 != label2 then boundary.break(false) // different label down the same route, not compat

            true // no contradictions found, this is compatible
        }

private object Route:
    def apply(doorLabels: Seq[(Int, Int)]): Route =
        val doors = doorLabels.map { case (roomLabel, doorIndex) => DoorDef(roomLabel, doorIndex) }
        Route(doors.map { door => door.doorIndex }, doors.map { door => door.roomLabel })

class FactEngine(rooms: Int, facts: FactCloud):

    def feed(doors: Seq[Int], roomLabels: Seq[Int]): FactEngine =
        val virtualRooms = produceVirtualRooms(doors, roomLabels)
        FactEngine(
            rooms,
            virtualRooms.foldLeft(facts) { (cloud, room) => cloud.addFact(room) }
        )

    def tryGuess(): Option[SolutionDefinition] = None

    private def produceVirtualRooms(doors: Seq[Int], roomLabels: Seq[Int]): Seq[VirtualRoom] =
        val firstLabel = roomLabels.head
        val route = Route(doors, roomLabels.tail)
        val firstRoom = VirtualRoom(
            Set(
                Fact.IsStartRoom,
                Fact.HasLabel(firstLabel),
                Fact.HasRoute(route)
            )
        )

        firstRoom +: route.tails.toSeq.map { lesserRoute => produceVirtualRoom(lesserRoute) }

    private def produceVirtualRoom(route: Route): VirtualRoom =
        val firstLabel = route.head.roomLabel
        val newRoute = route.tail
        VirtualRoom(
            Set(
                Fact.HasLabel(firstLabel),
                Fact.HasRoute(newRoute)
            )
        )


object FactEngine {
    def allocate(rooms: Int): FactEngine =
        FactEngine(rooms, FactCloud(Vector.empty))
}

private class RoomMapNode(val label: Int, val doors: Vector[RoomMapNode])

private enum Fact:
    case IsStartRoom
    case HasLabel(label: Int)
    case HasRoute(route: Route)

private class VirtualRoom(val facts: Set[Fact]):
    private val isStartRoom = facts.contains(Fact.IsStartRoom)
    private val label = facts.collectFirst { case x: Fact.HasLabel => x.label }
    private val existingDoors = facts
        .collect { case fact: HasRoute => fact }
        .map { fact => (fact.route.head.doorIndex, fact) }
        .toMap

    def compatibleWith(other: VirtualRoom): Boolean = other.facts.forall {
        case Fact.IsStartRoom => true
        case Fact.HasLabel(label) => this.label.isEmpty || this.label.contains(label)
        case Fact.HasRoute(route) =>
            val newDoor = route.head
            val existingDoor = existingDoors.get(newDoor.doorIndex)
            existingDoor match
                case Some(x) => x.route.compatibleWith(route)
                case None => true
    }

    def mergeWith(other: VirtualRoom) = VirtualRoom(facts ++ other.facts)

private class FactCloud(val rooms: Vector[VirtualRoom]):
    def addFact(virtualRoom: VirtualRoom): FactCloud =
        var anyCompatible = false
        var newRooms = rooms.map { room =>
            val isCompatible = room.compatibleWith(virtualRoom)
            anyCompatible = anyCompatible || isCompatible
            if isCompatible then room.mergeWith(virtualRoom) else room
        }
        if (!anyCompatible)
            newRooms = newRooms :+ virtualRoom

        FactCloud(newRooms)
