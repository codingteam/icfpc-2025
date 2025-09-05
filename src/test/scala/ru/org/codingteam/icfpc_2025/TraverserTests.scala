package ru.org.codingteam.icfpc_2025

import org.scalatest.funsuite.AnyFunSuite

class TraverserTests extends AnyFunSuite:

    test("simple route") {
        val solution = SolutionDefinition(
            Vector(0, 1, 2),
            0,
            Vector(
                ConnectionDefinition(Door(0, 0), Door(1, 0)),
                ConnectionDefinition(Door(1, 1), Door(2, 1)),
                ConnectionDefinition(Door(2, 2), Door(0, 2))
            )
        )
        val route = Seq(0, 1, 2)
        val result = Traverser.traverse(solution, route)
        assert(result == Seq(0, 1, 2, 0))
    }
