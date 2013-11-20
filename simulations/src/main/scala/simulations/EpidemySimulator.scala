package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
  }

  import SimConfig._

  val PrevalenceRate = 0.01
  val persons: List[Person] = for (
      i <- List.range(1, SimConfig.population+1)
    ) yield { new Person(i) }

  class Person (val id: Int) {
    val MoveDays = 5
    var infected = id <= (SimConfig.population * PrevalenceRate).toInt
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def moveAction {
      // println(s"I am moving $id")
    }

    afterDelay(randomBelow(MoveDays)+1)(moveAction)
  }
}
