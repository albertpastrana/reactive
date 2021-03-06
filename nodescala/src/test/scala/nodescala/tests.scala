package nodescala



import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)
    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A failed Future should fail") {
    intercept[Exception] {
      val fails = Future.failure[Int]
      Await.result(fails, 100 nanos)      
    }
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("all with some never should never complete") {
    val fs = List(Future.always(1), Future.never[Int], Future.never[Int], Future.never[Int])
    val res = Future.all(fs)
    try {
      Await.result(res, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("all with all success should return all results") {
    val fs = List(Future.always(1), Future.always(2), Future.always(3))
    val res = Future.all(fs)
    expectResult(List(1, 2, 3)) {
      Await.result(res, 100 nanos)
    }
  }

  test("all with a failure should return a failure") {
    val fs = List(Future.always(1), Future.always(2), Future.always(3), Future.failure[Int])
    intercept[Exception] {
      val res = Future.all(fs)
      Await.result(res, 100 nanos)
    }
  }

  //From https://class.coursera.org/reactive-001/forum/thread?thread_id=1254
  // test("all with a failure and never should never return") {
  //   val fs = List(Future.failure, Future.never[Int])

  //   val res = Future.all(fs)

  //   intercept[TimeoutException] {
  //     Await.result(res, 1 seconds)
  //   }
  // }

  test("any(always, never, never) should return always") {
    val fs = List(Future.always(1), Future.never[Int], Future.never[Int])
    val any = Future.any(fs)
    expectResult(1) {
      Await.result(any, 1 second)
    }
  }

  test("A Future should not complete after 500 millis when using a delay of 1s") {    
    intercept[TimeoutException] {
      val f = Future.delay(1 second)    
      Await.result(f, 500 millis)
    }
  }

  test("A Future should complete after 1s when using a delay of 500 millis") {
    val f2 = Future.delay(500 millis)
    Await.result(f2, 1 second)
    assert(true)
  }

  test("now of always should return correct value") {
    val always = Future.always(1)
    expectResult(1) {
      always.now
    }
  }

  test("now of never should throw NoSuchElementException") {
    val never = Future.never[Int]
    intercept[NoSuchElementException] {
      never.now
    }
  }

  test("continueWith: on success calls continuation") {
    val always = Future.always(1)
    val continuation = always.continueWith { f =>
        expectResult(true) { f.isCompleted }
        expectResult(always) { f }
        2
    }
    expectResult(2) {
      Await.result(continuation, 10 millis)
    }
  }

  test("continueWith: on failure calls continuation") {
    val failure = Future.failure[Int]
    val continuation = failure.continueWith { f =>
        expectResult(true) { f.isCompleted }
        expectResult(failure) { f }
        2
    }
    expectResult(2) {
      Await.result(continuation, 10 millis)
    }
  }

  test("continue should propagate successfull results") {
    val always = Future.always(1)
    val continuation = always.continue { v => v }
    expectResult(Success(1)) {
      Await.result(continuation, 10 millis)
    }
  } 

  test("continue should handle exception") {
    val always = Future.always(1)
    val continuation = always.continue(_ => throw new Exception())
    intercept[Exception] {
      Await.result(continuation, 10 millis)
    }
  } 

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("unsubricribing from a run should stop it") {
    var finished = false
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          Thread.sleep(10)
        }
        finished = true
      }
    }
    Future.delay(500 millis) onSuccess {
      case _ => working.unsubscribe()
    }
    Await.ready(Future.delay(600 millis), Duration.Inf)
    expectResult(true) {
      finished
    }
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




