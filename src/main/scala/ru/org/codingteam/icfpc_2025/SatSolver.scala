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
        var exploredConnections = Seq.empty[(Int, Int, Int)]

        for j <- knowledge.visitedRoutes.indices do
            val plan = knowledge.visitedRoutes(j)
            val rooms = knowledge.visitedRooms(j)

            for i <- plan.indices do
                val door = plan(i)
                val enter = rooms(i)
                val exit = rooms(i + 1)

                exploredConnections = exploredConnections :+ (enter, exit, door)

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
        for (enterLabel, exitLabel, door) <- exploredConnections do
            // enter
            for i <- 0 to problem.size - 1 do
                for j <- 0 to problem.size - 1 do
                    sb ++= s"-${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, door)} "
                    sb ++= s"${roomLabelVariableIndex(i, enterLabel)}"
            // exit
            for i <- 0 to problem.size - 1 do
                for j <- 0 to problem.size - 1 do
                    sb ++= s"-${roomLabelVariablesCount + connectionVariableIndex(problem.size, i, j, door)} "
                    sb ++= s"${roomLabelVariableIndex(j, exitLabel)}"

        Files.writeString(folder.resolve("step1.dimacs"), sb.toString)
        throw new Exception("Incorrect solution! Analyze results in \"path\".")

        if knowledge.visitedRoutes.size > 0 then
            return Step.StopGuessing()
        else
            val plan = Seq(Lanternarius.lanternarius(problem.maxRouteLength))
            return Step.ExploreStep(plan)

    private def roomLabelVariableIndex(room: Int, label: Int): Int = room*4 + label + 1

    private def connectionVariableIndex(roomsCount: Int, enterRoom: Int, exitRoom: Int, door: Int) =
        enterRoom*roomsCount*6 + exitRoom*6 + door + 1
