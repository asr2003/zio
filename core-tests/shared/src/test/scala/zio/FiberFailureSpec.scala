package zio

import zio.test._
import java.io.{ByteArrayOutputStream, PrintStream}

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
              ZIO.log(s"Unexpected failure: ${other.getMessage}") *>
                ZIO.succeed(s"Unexpected failure: ${other.getMessage}")
          }
          .asInstanceOf[ZIO[Any, Nothing, String]]

        fiberFailureTest.flatMap { stackTrace =>
          ZIO.log(s"Asserting stack trace:\n$stackTrace") *>
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
        def checkFailure(exit: Exit[Any, Any], expectedMessage: String): UIO[TestResult] = exit match {
          case Exit.Failure(cause) =>
            ZIO.log(s"Handling failure: $expectedMessage") *>
              ZIO
                .succeed(FiberFailure(cause.asInstanceOf[Cause[Any]]))
                .flatMap(verifyStackTraceConsistency(_, List(expectedMessage)))
          case Exit.Success(_) =>
            ZIO.log(s"Unexpected success for: $expectedMessage") *>
              ZIO.succeed(assertTrue(false) ?? s"Expected failure but got success for: $expectedMessage")
        }

        for {
          stringFailure <- ZIO.fail("string failure").exit.tap(exit => ZIO.log(s"String failure exit: $exit"))
          throwableFailure <- ZIO
                                .fail(new RuntimeException("throwable failure"))
                                .exit
                                .tap(exit => ZIO.log(s"Throwable failure exit: $exit"))
          dieFailure <- ZIO.die(new RuntimeException("die")).exit.tap(exit => ZIO.log(s"Die failure exit: $exit"))
          exitFail   <- ZIO.fail("exit fail").exit.tap(exit => ZIO.log(s"Exit fail exit: $exit"))
          exitDie    <- ZIO.die(new RuntimeException("exit die")).exit.tap(exit => ZIO.log(s"Exit die exit: $exit"))
          interruptFailure <-
            ZIO.interrupt.fork.flatMap(_.join.exit).tap(exit => ZIO.log(s"Interrupt failure exit: $exit"))
          results <- ZIO.collectAll(
                       List(
                         checkFailure(stringFailure, "string failure"),
                         checkFailure(throwableFailure, "throwable failure"),
                         checkFailure(dieFailure, "die"),
                         checkFailure(exitFail, "exit fail"),
                         checkFailure(exitDie, "exit die"),
                         checkFailure(interruptFailure, "interruption")
                       )
                     )
          _ <- ZIO.log(s"All results: $results")
        } yield assertTrue(results.forall(_.isSuccess))
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

    ZIO.log(s"Verifying stack trace consistency for:\n$stackTrace") *>
      ZIO.log(s"toString output:\n$toStringOutput") *>
      ZIO.log(s"printStackTrace output:\n$printStackTraceOutput") *>
      ZIO.succeed {
        val allStackTraces = List(stackTrace, toStringOutput, printStackTraceOutput)
        val matches = allStackTraces.forall { trace =>
          expectedStackTrace.forall(trace.contains)
        }
        assertTrue(matches)
      }
  }
}
