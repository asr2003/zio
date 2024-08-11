package zio

import zio._
import zio.test._
import java.io.{ByteArrayOutputStream, PrintStream}

object FiberFailureSpec extends ZIOSpecDefault {

  def spec = suite("FiberFailureSpec")(
    suite("FiberFailure stack trace handling")(
      test("captures the full stack trace for ZIO.fail with String and checks consistency across methods") {
        val fiberFailureTest = ZIO
          .fail("failure")
          .foldCauseZIO(
            cause => ZIO.succeed(FiberFailure(cause.asInstanceOf[Cause[Any]])),
            _ => ZIO.fail("Unexpected success")
          )

        fiberFailureTest.flatMap { fiberFailure =>
          val expectedStackTrace = List(
            "zio.FiberFailureSpec$.failTestMethod",
            "zio.FiberFailureSpec$.$anonfun$spec$1",
            "zio.internal.FiberRuntime.runLoop"
          )

          verifyStackTraceConsistency(fiberFailure, expectedStackTrace)
        }
      },
      test("captures the full stack trace for ZIO.fail with Throwable and checks consistency across methods") {
        val fiberFailureTest = ZIO
          .fail(new RuntimeException("failure"))
          .foldCauseZIO(
            cause => ZIO.succeed(FiberFailure(cause.asInstanceOf[Cause[Any]])),
            _ => ZIO.fail("Unexpected success")
          )

        fiberFailureTest.flatMap { fiberFailure =>
          val expectedStackTrace = List(
            "zio.FiberFailureSpec$.failThrowableTestMethod",
            "zio.FiberFailureSpec$.$anonfun$spec$2",
            "zio.internal.FiberRuntime.runLoop"
          )

          verifyStackTraceConsistency(fiberFailure, expectedStackTrace)
        }
      },
      test("captures the full stack trace for ZIO.die and checks consistency across methods") {
        val fiberFailureTest = ZIO
          .die(new RuntimeException("boom"))
          .foldCauseZIO(
            cause => ZIO.succeed(FiberFailure(cause.asInstanceOf[Cause[Any]])),
            _ => ZIO.fail("Unexpected success")
          )

        fiberFailureTest.flatMap { fiberFailure =>
          val expectedStackTrace = List(
            "zio.FiberFailureSpec$.dieTestMethod",
            "zio.FiberFailureSpec$.$anonfun$spec$3",
            "zio.internal.FiberRuntime.runLoop"
          )

          verifyStackTraceConsistency(fiberFailure, expectedStackTrace)
        }
      },
      test("captures the full stack trace for Exit.fail and checks consistency across methods") {
        val exit         = Exit.fail("failure")
        val fiberFailure = FiberFailure(exit.cause.asInstanceOf[Cause[Any]])
        val expectedStackTrace = List(
          "zio.FiberFailureSpec$.exitFailTestMethod",
          "zio.FiberFailureSpec$.$anonfun$spec$4",
          "zio.internal.FiberRuntime.runLoop"
        )

        verifyStackTraceConsistency(fiberFailure, expectedStackTrace)
      },
      test("captures the full stack trace for Exit.die and checks consistency across methods") {
        val exit         = Exit.die(new RuntimeException("boom"))
        val fiberFailure = FiberFailure(exit.cause.asInstanceOf[Cause[Any]])
        val expectedStackTrace = List(
          "zio.FiberFailureSpec$.exitDieTestMethod",
          "zio.FiberFailureSpec$.$anonfun$spec$5",
          "zio.internal.FiberRuntime.runLoop"
        )

        verifyStackTraceConsistency(fiberFailure, expectedStackTrace)
      },
      test("captures the full stack trace for ZIO.interrupt and checks consistency across methods") {
        val fiberId = FiberId.Runtime(0, 123, Trace.empty)
        val fiberFailureTest = ZIO
          .interruptAs(fiberId)
          .foldCauseZIO(
            cause => ZIO.succeed(FiberFailure(cause.asInstanceOf[Cause[Any]])),
            _ => ZIO.fail("Unexpected success")
          )

        fiberFailureTest.flatMap { fiberFailure =>
          val expectedStackTrace = List(
            "zio.FiberFailureSpec$.interruptTestMethod",
            "zio.FiberFailureSpec$.$anonfun$spec$6",
            "zio.internal.FiberRuntime.runLoop"
          )

          verifyStackTraceConsistency(fiberFailure, expectedStackTrace)
        }
      }
    )
  )

  private def verifyStackTraceConsistency(
    fiberFailure: FiberFailure,
    expectedStackTrace: List[String]
  ): UIO[TestResult] = {
    val stackTrace = fiberFailure.getStackTrace.mkString("\n")

    val toStringOutput = fiberFailure.toString
    val printStackTraceOutput = {
      val baos = new ByteArrayOutputStream()
      val ps   = new PrintStream(baos)
      fiberFailure.printStackTrace(ps)
      ps.flush()
      new String(baos.toByteArray)
    }

    val allStackTraces = List(stackTrace, toStringOutput, printStackTraceOutput)
    ZIO.succeed {
      assertTrue(
        allStackTraces.forall(trace => expectedStackTrace.forall(trace.toString.contains))
      )
    }
  }
}
