package ru.org.codingteam.icfpc_2025

import sttp.client4.upicklejson.default.asJson
import upickle.ReadWriter

import java.nio.file.{Files, Path}

object Solver {
    def solve(problem: ProblemDefinition): Unit = {
        println(s"Solving problem ${problem.name}.")
        Ædificium.select(problem.name)
        println(s"${problem.name} has been selected.")

        var knowledge = KnowledgeHolder(Vector.empty, Vector.empty)
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
        var ede = Map.empty[(Int, Int), Seq[Int]]

        for (j <- knowledge.visitedRoutes.indices) {
            val plan = knowledge.visitedRoutes(j)
            val rooms = knowledge.results(j)

            for (i <- plan.indices) {
                val door = plan(i)
                val enter = rooms(i)
                val exit = rooms(i + 1)

                val key = (enter, door)

                if (!ede.contains(key)) {
                    ede += (key -> Seq(exit))
                } else if (!ede(key).contains(exit)) {
                    ede += (key -> (ede(key) :+ exit))
                }
            }
        }

        val eded: Map[(Int, Int), Seq[(Int, Int)]] =
            ede.map { case (key @ (a, b), exits) =>
                val relatedKeys = ede.keys.filter { case (x, y) => exits.contains(x) && ede(x, y).contains(a) }.toSeq
                key -> relatedKeys
            }

        // I did not implemented it, but someone moore clever than me can implement the following approach:
        // 1) make a walk using routes and map of exit-door <-> possible enter-doors (eded var)
        // 2) fill solution where possible and 100% sure that the way is correct
        // 3) do not forget that there are also rooms
        // 4) find all missed keys
        // 5) determine walks to them
        // 6) generate paths to them + finish these routes with random
        // 7) go to 1

        if (knowledge.visitedRoutes.nonEmpty) return Step.StopGuessing()

        val plan = Seq(Lanternarius.lanternarius(problem.maxRouteLength))
        Step.ExploreStep(plan)

    private def dump(problem: ProblemDefinition, knowledge: KnowledgeHolder, solution: SolutionDefinition): Path =
        val folder = Files.createTempDirectory(s"icfpc.${problem.name}")
        Files.writeString(folder.resolve("knowledge.json"), asJson(knowledge).s)
        Files.writeString(folder.resolve("solution.json"), asJson(solution).s)

}

private def explore(problem: ProblemDefinition, knowledge: KnowledgeHolder, plans: Seq[Seq[Int]]): KnowledgeHolder =
    println("Exploring the labyrinth...")
    println("Plans:\n" + plans.map(_.mkString(" ")).mkString(start = "- ", sep = "\n", end = ""))
    val results = Ædificium.explore(plans)
    println("Exploration results:\n" + results.map(_.mkString(" ")).mkString(start = "- ", sep = "\n", end = ""))
    knowledge.incorporateKnowledge(plans, results)
