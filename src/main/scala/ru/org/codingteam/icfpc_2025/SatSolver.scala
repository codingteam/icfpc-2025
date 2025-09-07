package ru.org.codingteam.icfpc_2025

import sttp.client4.upicklejson.default.asJson
import upickle.ReadWriter

import java.nio.file.{Files, Path}

object SatSolver:
    def solve(problem: ProblemDefinition): Unit =
        println(s"Solving problem ${problem.name}.")
        Ædificium.select(problem.name)
        println(s"${problem.name} has been selected.")

        var knowledge = KnowledgeHolder(problem.size, Vector.empty, Vector.empty, Vector.empty, Vector.empty)
        while true do
            println("Determining next step...")
            val step = nextStep(problem, knowledge)
            println(s"Next step is ${step.getClass.getSimpleName}")
            step match
                case Step.ExploreStep(plans) =>
                    knowledge = explore(problem, knowledge, plans)
                case Step.GuessStep(solution) =>
                    println("Trying to commit the solution.")
                    val correct = Ædificium.guess(solution)
                    if correct then
                        println(s"The solution for problem ${problem.name} is correct.")
                        return
                    else
                        println(s"The solution for problem ${problem.name} is not correct!")
                        throw new Exception("Incorrect solution! Analyze results in \"path\".")
                case Step.StopGuessing() =>
                    println("Don't wanna play anymore.")
                    return

    private def nextStep(problem: ProblemDefinition, knowledge: KnowledgeHolder): Step =
        if knowledge.visitedRoutes.size == 0 then
            val plan = Seq(Lanternarius.shuffle12(problem.maxRouteLength))
            return Step.ExploreStep(plan)

        var exploredConnections = Seq.empty[(Int, Int, Int)]
        var byEnter = Map.empty[Int, Seq[(Int, Int)]]
        var byExit = Map.empty[Int, Seq[(Int, Int)]]
        var byEnterExit = Map.empty[(Int, Int), Seq[Int]]

        for j <- knowledge.visitedRoutes.indices do
            val plan = knowledge.visitedRoutes(j)
            val rooms = knowledge.visitedRooms(j)

            for i <- plan.indices do
                val door = plan(i)
                val enter = rooms(i)
                val exit = rooms(i + 1)

                exploredConnections = exploredConnections :+ (enter, exit, door)

                if !byEnter.contains(enter) then 
                    byEnter += (enter -> Seq((exit, door)))
                else
                    byEnter += (enter -> (byEnter(enter) :+ (exit, door)))

                if !byExit.contains(exit) then 
                    byExit += (exit -> Seq((enter, door)))
                else
                    byExit += (exit -> (byExit(exit) :+ (enter, door)))

                var enterExit = (enter, exit)
                if !byEnterExit.contains(enterExit) then 
                    byEnterExit += (enterExit -> Seq(door))
                else
                    byEnterExit += (enterExit -> (byEnterExit(enterExit) :+ door))

        println(s"explored conntection: ${exploredConnections}")

        val folder = Files.createTempDirectory(s"icfpc.sat.${problem.name}")
        println(s"Temp directory: ${folder}")

        val sb = new StringBuilder()
        val roomLabelVariablesCount = problem.size*4
        val roomLabelClausesCount = problem.size

        val connectionVariablesCount = problem.size*problem.size*6 + problem.size*6
        val connectionClausesCount = problem.size*6 + problem.size*problem.size*6

        val variablesCount = roomLabelVariablesCount + connectionVariablesCount
        val clausesCount = roomLabelClausesCount + connectionClausesCount
        sb ++= s"p cnf ${variablesCount} 9999\n"

        // rooms has one of {0, 1, 2, 3} labels
        for i <- 0 to problem.size - 1 do
            for l <- 0 to 3 do
                sb ++= s"${roomLabelVariableIndex(i, l)} "
            sb ++= "0\n"

        // but no more one label for the room
        for i <- 0 to problem.size - 1 do
            for k <- 0 to 3 do
                for l <- 0 to 3 do
                    if k < l then
                        sb ++= s"-${roomLabelVariableIndex(i, k)} "
                        sb ++= s"-${roomLabelVariableIndex(i, l)} "
                        sb ++= "0\n"

        // all labels should exist
        val maxLabel = if problem.size > 3 then 3 else problem.size-1
        for l <- 0 to maxLabel do
            for i <- 0 to problem.size - 1 do
                sb ++= s"${roomLabelVariableIndex(i, l)} "
            sb ++= "0\n"

        // for door d should exist at least one connection
        for i <- 0 to problem.size - 1 do
            for d <- 0 to 5 do
                for j <- 0 to problem.size - 1 do
                    sb ++= s"${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, d)} "
                sb ++= "0\n"

        // but no more than one
        for i <- 0 to problem.size - 1 do
            for d <- 0 to 5 do
                for k <- 0 to problem.size - 1 do
                    for l <- 0 to problem.size - 1 do
                        if k < l then
                            sb ++= s"-${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, k, d)} "
                            sb ++= s"-${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, l, d)} "
                            sb ++= "0\n"

        // no isolated rooms
        for i <- 0 to problem.size - 1 do
            for d <- 0 to 5 do
                sb ++= s"-${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, i, d)} "
            sb ++= "0\n"

        // there is should be at least one back connection OR forward connection should not exist
        for i <- 0 to problem.size - 1 do
            for j <- 0 to problem.size - 1 do
                for d <- 0 to 5 do
                    sb ++= s"-${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, d)} "
                    for k <- 0 to 5 do
                        sb ++= s"${roomLabelVariablesCount + connectionVariableIndex(problem.size, j, i, k)} "
                    sb ++= "0\n"

        // expeditions
        for (enter, exit, door) <- exploredConnections do
            if enter != exit then
                // exist connection between two different rooms through the __door__
                for i <- 0 to problem.size - 1 do
                    for j <- 0 to problem.size - 1 do
                        if i != j then
                            sb ++= s"${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, door)} "
                sb ++= "0\n"

        // for enter -door-> exit
        // if room labeled as __enter__ should exist one of connections to other rooms via the __door__
        for (enter, exits) <- byEnter do
            for i <- 0 to problem.size - 1 do
                sb ++= s"-${roomLabelVariableIndex(i, enter)} "

                for (exit, door) <- exits do
                    for j <- 0 to problem.size - 1 do
                        if enter != exit then
                            if i != j then
                                sb ++= s"${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, door)} "    
                        else
                            sb ++= s"${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, door)} "
                sb ++= "0\n"

        // if room labeled as __exit__ should exist one of connections from other rooms via the __door__
        for (exit, enters) <- byExit do
            for i <- 0 to problem.size - 1 do
                sb ++= s"-${roomLabelVariableIndex(i, exit)} "

                for (enter, door) <- enters do
                    for j <- 0 to problem.size - 1 do
                        if enter != exit then
                            if i != j then
                                sb ++= s"${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, door)} "    
                        else
                            sb ++= s"${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, door)} "
                sb ++= "0\n"

        for ((enter, exit), doors) <- byEnterExit do
            for i <- 0 to problem.size - 1 do
                for j <- 0 to problem.size - 1 do
                    if enter != exit then
                        if i != j then
                            sb ++= s"-${roomLabelVariableIndex(i, enter)} "
                            sb ++= s"-${roomLabelVariableIndex(j, exit)} "
                            for door <- doors do
                                sb ++= s"${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, door)} "    
                            sb ++= "0\n"
                    else
                        sb ++= s"-${roomLabelVariableIndex(i, enter)} "
                        sb ++= s"-${roomLabelVariableIndex(j, exit)} "
                        for door <- doors do
                            sb ++= s"${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, door)} "    
                        sb ++= "0\n"


        // first room label
        val (firstRoomLabel, firstExit, firstDoor) = exploredConnections.head
        sb ++= s"${roomLabelVariableIndex(0, firstRoomLabel)} 0\n"

        Files.writeString(folder.resolve("step1.dimacs"), sb.toString)
        throw new Exception("Incorrect solution! Analyze results in \"path\".")

    private def roomLabelVariableIndex(room: Int, label: Int): Int = room*4 + label + 1

    private def connectionVariableIndex(roomsCount: Int, enterRoom: Int, exitRoom: Int, door: Int) =
        enterRoom*roomsCount*6 + exitRoom*6 + door + 1
