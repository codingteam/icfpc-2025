package ru.org.codingteam.icfpc_2025

import sttp.client4.upicklejson.default.asJson
import upickle.ReadWriter

import java.nio.file.{Files, Path}

object Solver {
    def solve(problem: ProblemDefinition): Unit = {
        println(s"Solving problem ${problem.name}.")
        Ædificium.select(problem.name)
        println(s"${problem.name} has been selected.")

        var knowledge = KnowledgeHolder()
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
        if (knowledge.visited) return Step.StopGuessing()

        val plan = Seq(Lanternarius.lanternarius(problem.maxRouteLength))
        Step.ExploreStep(plan)

    private def dump(problem: ProblemDefinition, knowledge: KnowledgeHolder, solution: SolutionDefinition): Path =
        val folder = Files.createTempDirectory(s"icfpc.${problem.name}")
        Files.writeString(folder.resolve("knowledge.json"), asJson(knowledge).s)
        Files.writeString(folder.resolve("solution.json"), asJson(solution).s)

}

case class KnowledgeHolder(visited: Boolean = false) derives ReadWriter {
    def incorporateKnowledge(plans: Seq[Seq[Int]], results: Seq[Seq[Int]]): KnowledgeHolder =
        KnowledgeHolder(visited = true)
}

enum Step:
    case ExploreStep(plans: Seq[Seq[Int]])
    case GuessStep(solution: SolutionDefinition)
    case StopGuessing()

private def explore(problem: ProblemDefinition, knowledge: KnowledgeHolder, plans: Seq[Seq[Int]]): KnowledgeHolder =
    println("Exploring the labyrinth...")
    println("Plans:\n" + plans.map(_.mkString(" ")).mkString(start = "- ", sep = "\n", end = ""))
    val results = Ædificium.explore(plans)
    println("Exploration results:\n" + results.map(_.mkString(" ")).mkString(start = "- ", sep = "\n", end = ""))
    knowledge.incorporateKnowledge(plans, results)
