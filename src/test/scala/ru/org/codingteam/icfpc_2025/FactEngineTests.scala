package ru.org.codingteam.icfpc_2025

import org.scalatest.funsuite.AnyFunSuite

class FactEngineTests extends AnyFunSuite:
    test("by default it should not be resolved"):
        val engine = FactEngine.allocate(8)
        val solution = engine.tryGuess()
        assert(solution == None)

    test("it should guess"):
        var engine = FactEngine.allocate(2)
        //  000  000
        //  \|/  \|/
        //   A -- B
        //  /\   /\
        // 0 0  0 0
        val route = Seq     (0, 1, 3, 4, 5, 2, 0, 1, 2, 3, 5, 4)
        val results = Seq(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0)
        engine = engine.feed(route, results) 
        val solution = engine.tryGuess()
        assert(
            solution == SolutionDefinition(
                Seq(0, 1),
                0,
                Seq(
                    ConnectionDefinition(Door(0, 0), Door(0, 0)),
                    ConnectionDefinition(Door(0, 1), Door(0, 1)),
                    ConnectionDefinition(Door(0, 2), Door(1, 4)),
                    ConnectionDefinition(Door(0, 3), Door(0, 3)),
                    ConnectionDefinition(Door(0, 4), Door(0, 4)),
                    ConnectionDefinition(Door(0, 5), Door(0, 5)),
                    
                    ConnectionDefinition(Door(1, 0), Door(1, 0))),
                    ConnectionDefinition(Door(1, 1), Door(1, 1))),
                    ConnectionDefinition(Door(1, 2), Door(1, 2))),
                    // ConnectionDefinition(Door(1, 4), Door(0, 2))),
                    ConnectionDefinition(Door(1, 3), Door(1, 3)))
                    ConnectionDefinition(Door(1, 4), Door(1, 4)))
                    ConnectionDefinition(Door(1, 5), Door(1, 5)))
            )
        )
