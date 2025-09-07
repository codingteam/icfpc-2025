package ru.org.codingteam.icfpc_2025

import sttp.client4.upicklejson.default.asJson
import upickle.ReadWriter

import java.nio.file.{Files, Path}

object Solver {
    def solve(problem: ProblemDefinition): Unit = {
        println(s"Solving problem ${problem.name}.")
        Ædificium.select(problem.name)
        println(s"${problem.name} has been selected.")

        var knowledge = KnowledgeHolder(problem.size, Vector.empty, Vector.empty, Vector.empty, Vector.empty)
        while (true) {
            println("Determining next step...")
            val step = nextStep(problem, knowledge)
            println(s"Next step is ${step.getClass.getSimpleName}")
            step match
                case Step.ExploreStep(plans) =>
                    knowledge = explore(problem, knowledge, plans)
                case Step.GuessStep(solution) =>
                    println("Trying to commit the solution.")
                    val correct = Ædificium.guess(solution)
                    if (correct) {
                        println(s"The solution for problem ${problem.name} is correct.")
                        return
                    } else {
                        println(s"The solution for problem ${problem.name} is not correct!")
                        val path = dump(problem, knowledge, solution)
                        throw new Exception("Incorrect solution! Analyze results in \"path\".")
                    }
                case Step.StopGuessing() =>
                    println("Don't wanna play anymore.")
                    return
        }
    }

    private def nextStep(problem: ProblemDefinition, knowledge: KnowledgeHolder): Step =
        // (room, door in that room) -> (possible destination rooms)
        var roomAndDoorToPossibleRooms = Map.empty[(Int, Int), Seq[Int]]

        for (j <- knowledge.visitedRoutes.indices) {
            val plan = knowledge.visitedRoutes(j)
            val rooms = knowledge.visitedRooms(j)

            for (i <- plan.indices) {
                val door = plan(i)
                val roomFrom = rooms(i)
                val roomTo = rooms(i + 1)

                val key = (roomFrom, door)

                if (!roomAndDoorToPossibleRooms.contains(key)) {
                    roomAndDoorToPossibleRooms += (key -> Seq(roomTo))
                } else if (!roomAndDoorToPossibleRooms(key).contains(roomTo)) {
                    roomAndDoorToPossibleRooms += (key -> (roomAndDoorToPossibleRooms(key) :+ roomTo))
                }
            }
        }
        //for ((key, roomsTo) <- roomAndDoorToPossibleRooms)
        //    println(s"$key => $roomsTo")

        // (room) -> (room from which we could come into room in key, door in source room)
        var roomToPossibleRoomAndDoorFrom = Map.empty[Int, Set[(Int,Int)]]
        for ((key, roomsTo) <- roomAndDoorToPossibleRooms) {
           val roomFrom = key._1
           val relatedKeys : Set[(Int,Int)] = roomAndDoorToPossibleRooms.keys.filter { case (room, door) => roomsTo.contains(room) && roomAndDoorToPossibleRooms(room, door).contains(roomFrom) }.toSet
           if (!roomToPossibleRoomAndDoorFrom.contains(roomFrom)) {
               roomToPossibleRoomAndDoorFrom += (roomFrom -> relatedKeys)
           } else {
               roomToPossibleRoomAndDoorFrom += (roomFrom -> (roomToPossibleRoomAndDoorFrom(roomFrom) ++ relatedKeys))
           }
        }
        //for ((key, value) <- roomToPossibleRoomAndDoorFrom)
        //    println(s"$key => $value")

        // I did not implemented it, but someone moore clever than me can implement the following approach:
        // 1) make a walk using routes and map of exit-door <-> possible enter-doors (eded var)
        // 2) fill solution where possible and 100% sure that the way is correct
        // 3) do not forget that there are also rooms
        // 4) find all missed keys
        // 5) determine walks to them
        // 6) generate paths to them + finish these routes with random
        // 7) go to 1

        //if (knowledge.visitedRoutes.nonEmpty) return Step.StopGuessing()
        if (knowledge.visitedRoutes.nonEmpty)
            val res = DynamicSolver.processPlanAndRooms(
                MyGraph(knowledge.problemSize).setRoomLabel(0, Some(knowledge.lastExploreResult(0)(0))),
                0,
                knowledge.lastExploreRequest(0),
                knowledge.lastExploreResult(0)
            )
            val solution = res.get.toSolution
            Step.GuessStep(solution)
        else
            val plan = Seq(Lanternarius.shuffle12(problem.maxRouteLength))
            Step.ExploreStep(plan)

    private def dump(problem: ProblemDefinition, knowledge: KnowledgeHolder, solution: SolutionDefinition): Path =
        val folder = Files.createTempDirectory(s"icfpc.${problem.name}")
        Files.writeString(folder.resolve("knowledge.json"), asJson(knowledge).s)
        Files.writeString(folder.resolve("solution.json"), asJson(solution).s)

}

private def explore(problem: ProblemDefinition, knowledge: KnowledgeHolder, plans: Seq[Seq[Int]]): KnowledgeHolder =
    println("Exploring the labyrinth...")
    println("Plans:\n" + plans.map(_.mkString(" ")).mkString(start = "- ", sep = "\n", end = ""))
    val visitedRooms = Ædificium.explore(plans)
    println("Exploration results:\n" + visitedRooms.map(_.mkString(" ")).mkString(start = "- ", sep = "\n", end = ""))

    // dump data for Fortran solver
    /*
    for (i <- plans.indices) {
        val plan = plans(i)
        val room = visitedRooms(i)
        for (j <- plan.indices) {
            println(s"${room(j)} ${plan(j)} ${room(j+1)}")
        }
        println("")
    }
    */

    knowledge.incorporateKnowledge(plans, visitedRooms)
