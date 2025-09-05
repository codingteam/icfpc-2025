package ru.org.codingteam.icfpc_2025

@main def main(args: String*): Unit =
    args.toList match
        case "solve" :: problemName :: Nil =>
            println(s"solving $problemName")
            Ædificium.select(problemName)
            val result = Ædificium.explore(Seq(Seq(1, 2, 3)))
            println(result)
        case _ => println("Usage: sbt run \"solve <problem-name>\"")
