package zio

import zio.test._
import zio.test.Assertion._
import zio.test.TestAspect._
import java.io.{ByteArrayOutputStream, PrintStream}

object FiberFailureSpec extends ZIOBaseSpec {

  val expectedStackTrace = Seq(
    "zio.FiberSpec.spec.subcall",
    "java.base/java.lang.Thread.getStackTrace",
    "zio.FiberFailure.<init>",
    "zio.FiberFailure$.apply",
    "zio.Exit.$anonfun$getOrThrowFiberFailure$1",
    "zio.Exit.getOrElse",
    "zio.Exit.getOrElse$",
    "zio.Exit$Failure.getOrElse",
    "zio.Exit.getOrThrowFiberFailure",
    "zio.Exit.getOrThrowFiberFailure$",
    "zio.Exit$Failure.getOrThrowFiberFailure",
    "zio.FiberSpec$.$anonfun$spec$134",
    "zio.Unsafe$.unsafe",
    "zio.FiberSpec$.subcall$1",
    "zio.FiberSpec$.call$1",
    "zio.FiberSpec$.$anonfun$spec$136",
    "scala.runtime.java8.JFunction0$mcV$sp.apply",
    "zio.ZIOCompanionVersionSpecific.$anonfun$attempt$1",
    "zio.internal.FiberRuntime.runLoop",
    "zio.internal.FiberRuntime.evaluateEffect",
    "zio.internal.FiberRuntime.evaluateMessageWhileSuspended",
    "zio.internal.FiberRuntime.drainQueueOnCurrentThread",
    "zio.internal.FiberRuntime.run",
    "zio.internal.ZScheduler$$anon$3.run"
  )

  def validateStackTrace(stackTrace: String): Boolean =
    expectedStackTrace.forall(expected => stackTrace.contains(expected))

  def spec = suite("FiberFailureSpec")(
    test("FiberFailure getStackTrace includes relevant ZIO stack traces") {
      val exception    = new Exception("Test Exception")
      val fiberFailure = FiberFailure(Cause.fail(exception))
      val stackTrace   = fiberFailure.getStackTrace.map(_.toString).mkString("\n")

      assertTrue(validateStackTrace(stackTrace))

    },
    test("FiberFailure toString should match cause.prettyPrint") {
      val cause        = Cause.fail(new Exception("Test Exception"))
      val fiberFailure = FiberFailure(cause)

      assert(fiberFailure.toString)(equalTo(cause.prettyPrint))
    },
    test("FiberFailure printStackTrace should correctly output the stack trace") {
      val cause        = Cause.fail(new Exception("Test Exception"))
      val fiberFailure = FiberFailure(cause)

      val outputStream = new ByteArrayOutputStream()
      val printStream  = new PrintStream(outputStream)

      fiberFailure.printStackTrace(printStream)

      val stackTraceOutput = new String(outputStream.toByteArray)

      assertTrue(validateStackTrace(stackTraceOutput))

    },
    test("FiberFailure captures the stack trace for ZIO.fail with String") {
      def subcall(): Unit =
        Unsafe.unsafe { implicit unsafe =>
          Runtime.default.unsafe.run(ZIO.fail("boom")).getOrThrowFiberFailure()
        }
      def call1(): Unit = subcall()

      val fiberFailureTest = ZIO
        .attempt(call1())
        .catchAll {
          case fiberFailure: FiberFailure =>
            val stackTrace = fiberFailure.getStackTrace.mkString("\n")
            ZIO.log(s"Captured Stack Trace:\n$stackTrace") *>
              ZIO.succeed(stackTrace)
          case other =>
            ZIO.succeed(s"Unexpected failure: ${other.getMessage}")
        }
        .asInstanceOf[ZIO[Any, Nothing, String]]

      fiberFailureTest.flatMap { stackTrace =>
        ZIO.succeed {
          assertTrue(validateStackTrace(stackTrace))
        }
      }
    },
    test("FiberFailure captures the stack trace for ZIO.fail with Throwable") {
      def subcall(): Unit =
        Unsafe.unsafe { implicit unsafe =>
          Runtime.default.unsafe.run(ZIO.fail(new Exception("boom"))).getOrThrowFiberFailure()
        }
      def call1(): Unit = subcall()

      val fiberFailureTest = ZIO
        .attempt(call1())
        .catchAll {
          case fiberFailure: FiberFailure =>
            val stackTrace = fiberFailure.getStackTrace.mkString("\n")
            ZIO.log(s"Captured Stack Trace:\n$stackTrace") *>
              ZIO.succeed(stackTrace)
          case other =>
            ZIO.succeed(s"Unexpected failure: ${other.getMessage}")
        }
        .asInstanceOf[ZIO[Any, Nothing, String]]

      fiberFailureTest.flatMap { stackTrace =>
        ZIO.succeed {
          assertTrue(validateStackTrace(stackTrace))
        }
      }
    },
    test("FiberFailure captures the stack trace for ZIO.die") {
      def subcall(): Unit =
        Unsafe.unsafe { implicit unsafe =>
          Runtime.default.unsafe.run(ZIO.die(new RuntimeException("boom"))).getOrThrowFiberFailure()
        }
      def call1(): Unit = subcall()

      val fiberFailureTest = ZIO
        .attempt(call1())
        .catchAll {
          case fiberFailure: FiberFailure =>
            val stackTrace = fiberFailure.getStackTrace.mkString("\n")
            ZIO.log(s"Captured Stack Trace:\n$stackTrace") *>
              ZIO.succeed(stackTrace)
          case other =>
            ZIO.succeed(s"Unexpected failure: ${other.getMessage}")
        }
        .asInstanceOf[ZIO[Any, Nothing, String]]

      fiberFailureTest.flatMap { stackTrace =>
        ZIO.succeed {
          assertTrue(validateStackTrace(stackTrace))
        }
      }
    }
  ) @@ exceptJS
}
