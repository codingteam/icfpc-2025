package ru.org.codingteam.icfpc_2025

import sttp.client4.*
import sttp.client4.upicklejson.default.asJson
import upickle.ReadWriter

import java.nio.file.{Files, Path}
import scala.util.Using

object Ædificium {

    private val baseUrl = "https://31pwr5t6ij.execute-api.eu-west-2.amazonaws.com"

    private def backend = DefaultSyncBackend()
    private val id =
        val file = "api-secret.txt"
        if (Files.notExists(Path.of(file)))
            throw new Exception(s"File $file not found. Please create it and put your API secret there.")
        Using.resource(scala.io.Source.fromFile(file)) { source =>
            val secret = source.getLines().mkString("\n").trim
            secret
        }

    private case class SelectRequest(id: String, problemName: String) derives ReadWriter
    private case class ExploreRequest(id: String, plans: Seq[String]) derives ReadWriter
    private case class ExploreResponse(results: Seq[Seq[Int]], queryCount: Int) derives ReadWriter

    def select(problemName: String): Unit = {
        val response = basicRequest
            .post(uri"$baseUrl/select")
            .body(asJson(SelectRequest(id, problemName)))
            .send(backend)
        assertSuccess(response)
    }

    def explore(plans: Seq[Seq[Int]]): Seq[Seq[Int]] = {
        val request = ExploreRequest(id, plans.map { _.mkString("") })
        println(s"Sending request: ${asJson(request)}")

        val response = basicRequest
            .post(uri"$baseUrl/explore")
            .body(asJson(request))
            .response(asJson[ExploreResponse])
            .send(backend)
        assertSuccess(response)
        response.body.toOption.get.results
    }

    private def assertSuccess(response: Response[?]): Unit = {
        if response.code.code != 200 then
            throw RuntimeException(s"Request failed with status ${response.code.code}: ${response.body}")
    }
}

val AEdificium = Ædificium // accessibility! yay!
