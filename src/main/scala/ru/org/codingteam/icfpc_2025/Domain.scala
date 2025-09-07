package ru.org.codingteam.icfpc_2025

import upickle.ReadWriter

case class ProblemDefinition(name: String, size: Int, maxRouteLength: Int, firstBatch: Int)
object ProblemDefinition {
    def byName(name: String): ProblemDefinition =
        ProblemDefinition(
            name,
            name match
                case "probatio" => 3
                case "primus" => 6
                case "secundus" => 12
                case "tertius" => 18
                case "quartus" => 24
                case "quintus" => 30
                case _ => throw new Exception(s"Unknown problem name: $name."),
            name match
                case "probatio" => 12
                case "primus" => 108
                case "secundus" => 216
                case "tertius" => 324
                case "quartus" => 432
                case "quintus" => 540
                case _ => throw new Exception(s"Unknown problem name: $name."),
            name match
                case "probatio" => 1
                case "primus" => 1
                case "secundus" => 1
                case "tertius" => 1
                case "quartus" => 120
                case "quintus" => 150
                case _ => throw new Exception(s"Unknown problem name: $name."),
        )
}
case class SolutionDefinition(
    rooms: Seq[Int],
    startingRoom: Int,
    connections: Seq[ConnectionDefinition]
) derives ReadWriter
case class ConnectionDefinition(from: Door, to: Door) derives ReadWriter
case class Door(room: Int, door: Int) derives ReadWriter

case class KnowledgeHolder(
            problemSize: Int,
            visitedRoutes: Vector[Vector[Int]],
            visitedRooms: Vector[Vector[Int]],
            lastExploreRequest: Vector[Vector[Int]],
            lastExploreResult : Vector[Vector[Int]]
        ) derives ReadWriter {
    def incorporateKnowledge(plans: Seq[Seq[Int]], results: Seq[Seq[Int]]): KnowledgeHolder =
        KnowledgeHolder(
            this.problemSize,
            this.visitedRoutes ++ plans.map(_.toVector),
            this.visitedRooms ++ results.map(_.toVector),
            plans.map(_.toVector).toVector,
            results.map(_.toVector).toVector
        )
}

enum Step:
    case ExploreStep(plans: Seq[Seq[Int]])
    case GuessStep(solution: SolutionDefinition)
    case StopGuessing()
