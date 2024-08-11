package zio

import zio.test.TestAspect._
import zio.test._
import zio.test.Assertion._
import java.io.{ByteArrayOutputStream, PrintStream}

object FiberFailureSpec extends ZIOBaseSpec {
  def spec = suite("FiberFailureSpec")(
    test("FiberFailure getStackTrace includes both ZIO and Java stack traces") {
      val fiberFailure = FiberFailure(Cause.fail(new Exception("Test Exception")))
      val stackTrace   = fiberFailure.getStackTrace

      assertTrue(
        stackTrace.exists(_.getClassName.contains("FiberFailureSpec")),
        stackTrace.exists(_.getClassName.contains("Exception"))
      )
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

      assertTrue(
        stackTraceOutput.contains("FiberFailure"),
        stackTraceOutput.contains("Test Exception")
      )
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
          assertTrue(
            stackTrace.contains("call1") &&
              stackTrace.contains("subcall") &&
              stackTrace.contains("FiberFailureSpec")
          )
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
          assertTrue(
            stackTrace.contains("call1") &&
              stackTrace.contains("subcall") &&
              stackTrace.contains("FiberFailureSpec")
          )
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
          assertTrue(
            stackTrace.contains("call1") &&
              stackTrace.contains("subcall") &&
              stackTrace.contains("FiberFailureSpec")
          )
        }
      }
    },
    // test("FiberFailure captures the stack trace for Exit.fail") {
    //   def subcall(): Unit =
    //     Unsafe.unsafe { implicit unsafe =>
    //       val exit = ZIO.fail("boom").exit
    //       exit match {
    //         case Exit.Failure(cause) => throw FiberFailure(cause)
    //         case Exit.Success(_)     => ()
    //         case _                   => ()
    //       }
    //     }
    //   def call1(): Unit = subcall()

    //   val fiberFailureTest = ZIO
    //     .attempt(call1())
    //     .catchAll {
    //       case fiberFailure: FiberFailure =>
    //         val stackTrace = fiberFailure.getStackTrace.mkString("\n")
    //         ZIO.succeed(stackTrace)
    //       case other =>
    //         ZIO.succeed(s"Unexpected failure: ${other.getMessage}")
    //     }

    //   fiberFailureTest.flatMap { stackTrace =>
    //     ZIO.succeed {
    //       assertTrue(
    //         stackTrace.contains("call1") &&
    //           stackTrace.contains("subcall") &&
    //           stackTrace.contains("FiberFailureSpec")
    //       )
    //     }
    //   }
    // }
  ) @@ exceptJS
}
