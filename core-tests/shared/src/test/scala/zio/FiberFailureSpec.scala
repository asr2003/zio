package zio

import zio._

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
      }
    )
}
