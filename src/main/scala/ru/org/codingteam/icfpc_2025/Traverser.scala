package ru.org.codingteam.icfpc_2025

object Traverser:
    def traverse(solution: SolutionDefinition, route: Seq[Int]): Seq[Int] =
        var currentRoomIndex = solution.startingRoom
        var result = List(solution.rooms(currentRoomIndex))
        for (openDoor <- route)
            val fromConnection = solution.connections
                .filter { conn => conn.from.room == currentRoomIndex && conn.from.door == openDoor }
                .toVector
            val toConnection = solution.connections
                .filter { conn => conn.to.room == currentRoomIndex && conn.to.door == openDoor }
                .toVector

            if (fromConnection.size > 1 || toConnection.size > 1)
                throw new Exception(s"Invalid solution: found more than one connection for door $openDoor in room $currentRoomIndex.")

            if (fromConnection.nonEmpty && toConnection.nonEmpty && fromConnection.head != toConnection.head)
                throw new Exception(s"Invalid solution: connection $fromConnection does not match $toConnection (room $currentRoomIndex, door $openDoor).")

            if (fromConnection.isEmpty && toConnection.isEmpty)
                throw new Exception(s"Invalid solution: cannot find a door $openDoor in room $currentRoomIndex.")

            val connection = if fromConnection.nonEmpty then fromConnection.head else toConnection.head
            currentRoomIndex = if connection.from.room == currentRoomIndex then connection.to.room else connection.from.room
            result = currentRoomIndex :: result

        result.reverse
