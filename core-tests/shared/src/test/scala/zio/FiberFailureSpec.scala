package zio

import zio.test._
import zio.test.TestAspect._

object FiberFailureSpec extends ZIOBaseSpec {

  def spec = suite("FiberFailureSpec")(
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
          assertTrue(
            stackTrace.contains("call1") &&
              stackTrace.contains("subcall") &&
              stackTrace.contains("FiberFailureSpec")
          )
        }
      }
    } @@ exceptJS,
      test ("FiberFailure captures the stack trace for ZIO.fail with Throwable") {
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
            assertTrue(
              stackTrace.contains("call1") &&
                stackTrace.contains("subcall") &&
                stackTrace.contains("FiberFailureSpec")
            )
          }
        }
      } @@ exceptJS
  )
}
