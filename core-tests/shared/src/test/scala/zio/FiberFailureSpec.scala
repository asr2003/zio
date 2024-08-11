package zio

import zio.test.Assertion._
import zio.test._

object FiberFailureSpec extends ZIOBaseSpec {

  def spec =
    suite("FiberFailureSpec")(
      test("captures the full stack trace including user code") {
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

        fiberFailureTest.flatMap { stackTrace =>
          ZIO.succeed {
            assertTrue(
              stackTrace.contains("call1") &&
                stackTrace.contains("subcall") &&
                stackTrace.contains("FiberFailureSpec")
            )
          }
        }
      },
      test("handles different failure modes") {
        val stringFailureTest = for {
          exit        <- ZIO.fail("string failure").exit
          fiberFailure = FiberFailure(exit.cause)
        } yield verifyStackTraceConsistency(fiberFailure, List("string failure"))

        val throwableFailureTest = for {
          exit        <- ZIO.fail(new RuntimeException("throwable failure")).exit
          fiberFailure = FiberFailure(exit.cause)
        } yield verifyStackTraceConsistency(fiberFailure, List("throwable failure"))

        val dieTest = for {
          exit        <- ZIO.die(new RuntimeException("die")).exit
          fiberFailure = FiberFailure(exit.cause)
        } yield verifyStackTraceConsistency(fiberFailure, List("die"))

        val exitFailTest = for {
          exit        <- ZIO.succeed(Exit.fail("exit fail"))
          fiberFailure = FiberFailure(exit.cause)
        } yield verifyStackTraceConsistency(fiberFailure, List("exit fail"))

        val exitDieTest = for {
          exit        <- ZIO.succeed(Exit.die(new RuntimeException("exit die")))
          fiberFailure = FiberFailure(exit.cause)
        } yield verifyStackTraceConsistency(fiberFailure, List("exit die"))

        val interruptTest = for {
          fiber       <- ZIO.interrupt.fork
          exit        <- fiber.join.exit
          fiberFailure = FiberFailure(exit.cause)
        } yield verifyStackTraceConsistency(fiberFailure, List("interruption"))

        stringFailureTest *>
          throwableFailureTest *>
          dieTest *>
          exitFailTest *>
          exitDieTest *>
          interruptTest
      },
      test("consistency of getStackTrace, toString, and printStackTrace methods") {
        def subcall(): Unit =
          Unsafe.unsafe { implicit unsafe =>
            Runtime.default.unsafe.run(ZIO.fail("boom")).getOrThrowFiberFailure()
          }
        def call1(): Unit = subcall()

        val expectedStackTrace = List("call1", "subcall", "FiberFailureSpec")

        val fiberFailureTest = ZIO
          .attempt(call1())
          .catchAll { case fiberFailure: FiberFailure =>
            verifyStackTraceConsistency(fiberFailure, expectedStackTrace)
          }

        fiberFailureTest
      }
    )

  private def verifyStackTraceConsistency(
    fiberFailure: FiberFailure,
    expectedStackTrace: List[String]
  ): UIO[TestResult] = {
    val stackTrace     = fiberFailure.getStackTrace.mkString("\n")
    val toStringOutput = fiberFailure.toString
    val printStackTraceOutput = {
      val baos = new ByteArrayOutputStream()
      val ps   = new PrintStream(baos)
      fiberFailure.printStackTrace(ps)
      ps.flush()
      new String(baos.toByteArray)
    }

    val allStackTraces           = List(stackTrace, toStringOutput, printStackTraceOutput)
    val allTracesContainExpected = allStackTraces.forall(trace => expectedStackTrace.forall(trace.contains(_)))

    ZIO.succeed(assert(allTracesContainExpected))
  }
}
