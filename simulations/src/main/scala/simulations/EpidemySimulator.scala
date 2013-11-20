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

  def personsInRoom(row: Int, col:Int): List[Person] = persons.filter(_.inRoom(row, col))

  def anySickInRoom(row: Int, col:Int): Boolean = personsInRoom(row, col).exists(_.visibleSick)

  def anyInfectedInRoom(row: Int, col:Int): Boolean = personsInRoom(row, col).exists(_.infected)

  object Neighbours extends Enumeration {
    type Neighbour = Value
    val Left, Right, Up, Down = Value
    def any = Neighbours(randomBelow(4))
  }

  class Person (val id: Int) {
    val RndMoveDays = 5
    val SickDelay = 6
    val DieRate = 25
    val DeadDelay = 14
    val ImmuneDelay = 16
    val HealthyDelay = 18
    val TransmissibilityRate = 40
    var infected = id <= (SimConfig.population * PrevalenceRate).toInt
    var sick = false
    var immune = false
    var dead = false
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    private def moveAction {
      if (dead) return
      val safe = safeNeighbours
      if (safe.isEmpty) return
      val (newRow, newCol) = safe(randomBelow(safe.size))
      enterRoom(newRow, newCol)
    }

    private def enterRoom(row: Int, col: Int) {
      this.row = row
      this.col = col
      if (anyInfectedInRoom(row, col)) {
        if (randomBelow(100) < TransmissibilityRate) {
          infected = true
          infectedAction
        }        
      }
      nextMove
    }

    val neighbours = List(
      (row: Int, col: Int) => (row, (col + SimConfig.roomColumns - 1) % SimConfig.roomColumns),
      (row: Int, col: Int) => (row, (col + 1) % SimConfig.roomColumns),
      (row: Int, col: Int) => ((row + 1) % SimConfig.roomRows, col),
      (row: Int, col: Int) => ((row + SimConfig.roomRows - 1) % SimConfig.roomRows, col)
    )

    private def safeNeighbours: List[(Int, Int)] = {
      neighbours.map{ n => n(row, col) }.filter{ r => anyInfectedInRoom(r._1, r._2) }
    }

    private def infectedAction {
      if (dead) return
      def mustDie = randomBelow(100) < DieRate
      afterDelay(SickDelay)(sickAction)
      if (mustDie) {
        afterDelay(DeadDelay)(deadAction)
      } else {
        afterDelay(ImmuneDelay)(immuneAction)        
        afterDelay(HealthyDelay)(healthyAction)        
      }
    }

    private def sickAction {
      if (dead) return
      sick = true
    }

    private def deadAction {
      if (dead) return
      dead = true
      sick = true
    }

    private def immuneAction {
      if (dead) return
      immune = true
      sick = false
    }
    private def healthyAction {
      if (dead) return
      immune = false
      sick = false
    }

    def inRoom(row:Int, col:Int) = this.row == row && this.col == col
    def visibleSick = this.sick && !this.dead

    private def nextMove = afterDelay(randomBelow(RndMoveDays)+1)(moveAction)

    nextMove
    if (infected) {
      infectedAction
    }
  }
}
