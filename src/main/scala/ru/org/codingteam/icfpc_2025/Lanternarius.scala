package ru.org.codingteam.icfpc_2025

import scala.util.Random

object Lanternarius {
  def lanternarius(N: Int): Seq[Int] = {
    val rand = new Random()
    val vals = scala.collection.mutable.ArrayBuffer[Int]()

    var prev = rand.nextInt(6)
    vals += prev

    for (_ <- 1 until N) {
      if (rand.nextInt(100) < 15 &&
          (vals.size < 2 || vals(vals.size - 1) != vals(vals.size - 2))) {
        vals += prev
      } else {
        var next = rand.nextInt(6)
        while (next == prev) {
          next = rand.nextInt(6)
        }
        vals += next
        prev = next
      }
    }

    vals.toSeq
  }

  def shuffle6(N: Int): Seq[Int] = {
    val rand = new Random()
    val chunks = N / 6

    (1 to chunks).flatMap { _ =>
      rand.shuffle(0 to 5)
    }
  }

  def shuffle12(N: Int): Seq[Int] = {
    val rand = new Random()
    val chunks = N / 12
    val hasRemainder = (N % 12 == 6)

    val block = (0 to 5).flatMap(x => Seq(x, x))

    val seq = (1 to chunks).flatMap { _ =>
      rand.shuffle(block)
    }

    if (hasRemainder) seq ++ rand.shuffle(0 to 5) else seq
  }

  def lanternariuses(Nroutes: Int, length: Int): Seq[Seq[Int]] = {
    Seq.fill(Nroutes)(lanternarius(length))
  }
}
