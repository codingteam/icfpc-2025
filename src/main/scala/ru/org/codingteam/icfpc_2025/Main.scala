package ru.org.codingteam.icfpc_2025

@main def main(args: String*): Unit =
    args.toList match
        case "solve" :: "solver" :: problemName :: Nil =>
            val problem = ProblemDefinition.byName(problemName)
            Solver.solve(problem)
        case "solve" :: "sat" :: problemName :: Nil =>
            val problem = ProblemDefinition.byName(problemName)
            SatSolver.solve(problem)
        case _ => println("Usage: sbt run \"solve <problem-name>\"")
