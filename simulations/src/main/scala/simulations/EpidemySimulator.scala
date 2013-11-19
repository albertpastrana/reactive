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

  val persons: List[Person] = for (
      i <- List.range(1, SimConfig.population)
    ) yield { new Person(i) }

  agenda = List(
      new WorkItem(1, () => { println("action") } )
    )

  class Person (val id: Int) {
    var infected = randomBelow(100) < 10
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
  }
}
