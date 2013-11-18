package simulations

import org.scalatest.FunSuite
import org.scalatest.Assertions._ 

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "0 or 0")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "1 or 0")

    in2.setSignal(true)
    run    
    assert(out.getSignal === true, "1 or 1")

    in1.setSignal(false)
    run
    assert(out.getSignal === true, "0 or 1")
  }

  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)

    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "0 or2 0")

    in1.setSignal(true)
    run
    assert(out.getSignal === true, "1 or2 0")

    in2.setSignal(true)
    run    
    assert(out.getSignal === true, "1 or2 1")

    in1.setSignal(false)
    run
    assert(out.getSignal === true, "0 or2 1")
  }

  test("demux: simple: 0 control, 1 output") {
    val in, out = new Wire
    demux(in, List(), List(out))

    in.setSignal(false)
    run
    assert(out.getSignal === false, "in=0, c=-")

    in.setSignal(true)
    run
    assert(out.getSignal === true, "in=1, c=-")
  }

  test("demux: simple: 1 control, 2 outputs") {
    val in, c, out0, out1 = new Wire
    demux(in, List(c), List(out1, out0))

    expectResult(true, "in=1, c=0") {
      in.setSignal(true)
      c.setSignal(false)
      run
      out0.getSignal
    }

    expectResult(true, "in=1, c=1") {
      c.setSignal(true)
      run
      out1.getSignal
    }
  }

  test("demux: big: 2 control, 4 outputs") {
    val in, c1, c0 = new Wire
    val out = List(new Wire, new Wire, new Wire, new Wire)
    demux(in, List(c1, c0), out)

    in.setSignal(true)

    expectResult("0001", "in=1, c=00") {
      c0.setSignal(false)
      c1.setSignal(false)
      run      
      out.foldLeft("")((acc, w) => acc+w )
    }

    expectResult("0010", "in=1, c=01") {
      c0.setSignal(true)
      c1.setSignal(false)
      run      
      out.foldLeft("")((acc, w) => acc+w )
    }

    expectResult("0100", "in=1, c=10") {
      c0.setSignal(false)
      c1.setSignal(true)
      run      
      out.foldLeft("")((acc, w) => acc+w )
    }

    expectResult("1000", "in=1, c=11") {
      c0.setSignal(true)
      c1.setSignal(true)
      run      
      out.foldLeft("")((acc, w) => acc+w )
    }
  }
}