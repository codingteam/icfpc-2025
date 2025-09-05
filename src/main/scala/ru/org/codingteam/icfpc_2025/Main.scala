package ru.org.codingteam.icfpc_2025

@main def main(args: String*): Unit =
    args.toList match
        case "solve" :: problemName :: Nil =>
            val problem = ProblemDefinition.byName(problemName)
            Solver.solve(problem)
        case _ => println("Usage: sbt run \"solve <problem-name>\"")
