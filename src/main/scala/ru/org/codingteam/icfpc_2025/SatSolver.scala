package ru.org.codingteam.icfpc_2025

import sttp.client4.upicklejson.default.asJson
import upickle.ReadWriter

import java.nio.file.{Files, Path}
import scala.sys.process.Process
import scala.collection.mutable.ArrayBuffer

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

        println(s"Explored conntections: ${exploredConnections}")

        val sb = new StringBuilder()

        // in case of probatio there are only {0, 1, 2} labels
        // for other problems labels are {0, 1, 2, 3}
        val labelsCount = if problem.size > 3 then 4 else problem.size
        val doorsCount = 6

        val roomLabelVariablesCount = problem.size*labelsCount
        val connectionVariablesCount = problem.size*problem.size*doorsCount

        val variablesCount = roomLabelVariablesCount + connectionVariablesCount
        sb ++= s"p cnf ${variablesCount} 9999\n"

        def roomLabelVariableIndex(room: Int, label: Int): Int = room*labelsCount + label + 1
        def connectionVariableIndex(roomsCount: Int, enterRoom: Int, exitRoom: Int, door: Int) =
            roomLabelVariablesCount + enterRoom*roomsCount*doorsCount + exitRoom*doorsCount + door + 1

        // rooms has one of labels
        for i <- 0 to problem.size - 1 do
            for l <- 0 to labelsCount - 1 do
                sb ++= s"${roomLabelVariableIndex(i, l)} "
            sb ++= "0\n"

        // but no more one label for the room
        for i <- 0 to problem.size - 1 do
            for k <- 0 to labelsCount - 1 do
                for l <- 0 to labelsCount - 1 do
                    if k < l then
                        sb ++= s"-${roomLabelVariableIndex(i, k)} "
                        sb ++= s"-${roomLabelVariableIndex(i, l)} "
                        sb ++= "0\n"

        // all labels should exist
        for l <- 0 to labelsCount - 1 do
            for i <- 0 to problem.size - 1 do
                sb ++= s"${roomLabelVariableIndex(i, l)} "
            sb ++= "0\n"

        // for door d should exist at least one connection
        for i <- 0 to problem.size - 1 do
            for d <- 0 to doorsCount - 1 do
                for j <- 0 to problem.size - 1 do
                    sb ++= s"${connectionVariableIndex(problem.size, i, j, d)} "
                sb ++= "0\n"

        // but no more than one
        for i <- 0 to problem.size - 1 do
            for d <- 0 to doorsCount - 1 do
                for k <- 0 to problem.size - 1 do
                    for l <- 0 to problem.size - 1 do
                        if k < l then
                            sb ++= s"-${connectionVariableIndex(problem.size, i, k, d)} "
                            sb ++= s"-${connectionVariableIndex(problem.size, i, l, d)} "
                            sb ++= "0\n"

        // no isolated rooms
        for i <- 0 to problem.size - 1 do
            for d <- 0 to doorsCount - 1 do
                sb ++= s"-${connectionVariableIndex(problem.size, i, i, d)} "
            sb ++= "0\n"

        // first room label
        val (firstRoomLabel, firstExit, firstDoor) = exploredConnections.head
        sb ++= s"${roomLabelVariableIndex(0, firstRoomLabel)} 0\n"

        // there is should be at least one back connection OR forward connection should not exist
        for i <- 0 to problem.size - 1 do
            for j <- 0 to problem.size - 1 do
                for d <- 0 to doorsCount - 1 do
                    sb ++= s"-${connectionVariableIndex(problem.size, i, j, d)} "
                    for k <- 0 to doorsCount - 1 do
                        sb ++= s"${connectionVariableIndex(problem.size, j, i, k)} "
                    sb ++= "0\n"

        // same connections count from room i to j and room j to i
        def getBit(number: Int, position: Int): Int =
            (number >> position) & 1

        for i <- 0 to problem.size - 1 do
            for j <- 0 to problem.size - 1 do
                if i < j then
                    for k <- 1 to 63 do
                        for l <- 1 to 63 do
                            if Integer.bitCount(k) != Integer.bitCount(l) then
                                for d <- 0 to doorsCount - 1 do
                                    if getBit(k, d) > 0 then
                                        sb ++= s"-${connectionVariableIndex(problem.size, i, j, d)} "
                                for d <- 0 to doorsCount - 1 do
                                    if getBit(l, d) > 0 then
                                        sb ++= s"-${connectionVariableIndex(problem.size, j, i, d)} "
                                sb ++= "0\n"

        // // expeditions clauses
        // for (enter, exit, door) <- exploredConnections do
        //     if enter != exit then
        //         // exist connection between two different rooms through the __door__
        //         for i <- 0 to problem.size - 1 do
        //             for j <- 0 to problem.size - 1 do
        //                 if i != j then
        //                     sb ++= s"${connectionVariableIndex(problem.size, i, j, door)} "
        //         sb ++= "0\n"

        // // for enter -door-> exit
        // // if room labeled as __enter__ should exist one of connections to other rooms via the __door__
        // for (enter, exits) <- byEnter do
        //     for i <- 0 to problem.size - 1 do
        //         sb ++= s"-${roomLabelVariableIndex(i, enter)} "

        //         for (exit, door) <- exits do
        //             for j <- 0 to problem.size - 1 do
        //                 if enter != exit then
        //                     if i != j then
        //                         sb ++= s"${connectionVariableIndex(problem.size, i, j, door)} "    
        //                 else
        //                     sb ++= s"${connectionVariableIndex(problem.size, i, j, door)} "
        //         sb ++= "0\n"

        // // if room labeled as __exit__ should exist one of connections from other rooms via the __door__
        // for (exit, enters) <- byExit do
        //     for i <- 0 to problem.size - 1 do
        //         sb ++= s"-${roomLabelVariableIndex(i, exit)} "

        //         for (enter, door) <- enters do
        //             for j <- 0 to problem.size - 1 do
        //                 if enter != exit then
        //                     if i != j then
        //                         sb ++= s"${connectionVariableIndex(problem.size, i, j, door)} "    
        //                 else
        //                     sb ++= s"${connectionVariableIndex(problem.size, i, j, door)} "
        //         sb ++= "0\n"

        // for ((enter, exit), doors) <- byEnterExit do
        //     for i <- 0 to problem.size - 1 do
        //         for j <- 0 to problem.size - 1 do
        //             if enter != exit then
        //                 if i != j then
        //                     sb ++= s"-${roomLabelVariableIndex(i, enter)} "
        //                     sb ++= s"-${roomLabelVariableIndex(j, exit)} "
        //                     for door <- doors do
        //                         sb ++= s"${connectionVariableIndex(problem.size, i, j, door)} "    
        //                     sb ++= "0\n"
        //             else
        //                 sb ++= s"-${roomLabelVariableIndex(i, enter)} "
        //                 sb ++= s"-${roomLabelVariableIndex(j, exit)} "
        //                 for door <- doors do
        //                     sb ++= s"${connectionVariableIndex(problem.size, i, j, door)} "    
        //                 sb ++= "0\n"

        val folder = Files.createTempDirectory(s"icfpc.sat.${problem.name}")
        println(s"Temp directory: ${folder}")

        var step = 0
        var invalid = true

        while invalid && (step < 1) do
            invalid = false
            step = step + 1

            val cnfPath = folder.resolve(s"step${step}.cnf")
            val resultPath = folder.resolve(s"step${step}_res.txt")

            Files.writeString(cnfPath, sb.toString)
            val process = Process(s"minisat ${cnfPath.toAbsolutePath().toString()} ${resultPath.toAbsolutePath().toString()}")
            val exitCode = process.!

            val resultLines = Files.readAllLines(resultPath)
            val result = resultLines.get(1)
            println(s"${result}")

            val solution = result.split(" ").map(_.toInt)

            println("Checking if solution is valid...")
            for i <- 0 to problem.size - 1 do
                for j <- 0 to problem.size - 1 do
                    if i != j then
                        var outConnectionsCount = 0
                        for d <- 0 to 5 do
                            if solution(connectionVariableIndex(problem.size, i, j, d) - 1) > 0 then 
                                // println(s"out beta(${i}, ${j}, ${d}) = ${solution(connectionVariableIndex(problem.size, i, j, d) - 1)}")
                                outConnectionsCount = outConnectionsCount + 1

                        var inConnectionsCount = 0
                        for d <- 0 to 5 do
                            if solution(connectionVariableIndex(problem.size, j, i, d) - 1) > 0 then
                                // println(s"in beta(${j}, ${i}, ${d}) = ${solution(connectionVariableIndex(problem.size, j, i, d) - 1)}")
                                inConnectionsCount = inConnectionsCount + 1

                        // println(s"rooms ${i} ${j}, out = ${outConnectionsCount}")
                        // println(s"rooms ${i} ${j}, in = ${inConnectionsCount}")
                        if outConnectionsCount != inConnectionsCount then
                            invalid = true

            if invalid then
                println("INVALID")
                for v <- solution do
                    sb ++= s"${-v} "
                sb ++= "\n"

        println(s"Explored conntections: ${exploredConnections}")
        println(s"Temp directory: ${folder}")
        throw new Exception("Incorrect solution! Analyze results in \"path\".")
