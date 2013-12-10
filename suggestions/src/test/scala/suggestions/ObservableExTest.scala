package suggestions


import scala.collection._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import scala.swing.event.Event
import scala.swing.Reactions.Reaction
import rx.lang.scala._
import org.scalatest._
import observablex._

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ObservableExTest extends FunSuite {

  test("Future should give result on Observable") {
    val f = Future(5)
    val o = ObservableEx(f)
    assert(o.toBlockingObservable.toList === List(5))
  }
  
  test("Future may throw on Observable") {
    val f = Future.failed(new Exception("test"))
    val o = ObservableEx(f)
    intercept[Exception] {
      o.toBlockingObservable.toList
    }  
  }
}