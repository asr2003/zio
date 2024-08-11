package zio

import zio.test._

object FiberFailureSpec extends ZIOBaseSpec {

  def spec =
    suite("FiberFailureSpec")(
      test("captures full stack trace including user code for ZIO.fail with String") {
        def subcall(): Unit =
          Unsafe.unsafe { implicit unsafe =>
            Runtime.default.unsafe.run(ZIO.fail("boom")).getOrThrowFiberFailure()
          }
        def call1(): Unit = subcall()

        assertStackTraceConsistency(() => call1())
      },
      test("captures full stack trace including user code for ZIO.fail with Throwable") {
        def subcall(): Unit =
          Unsafe.unsafe { implicit unsafe =>
            Runtime.default.unsafe.run(ZIO.fail(new RuntimeException("boom"))).getOrThrowFiberFailure()
          }
        def call1(): Unit = subcall()

        assertStackTraceConsistency(() => call1())
      },
      test("captures full stack trace including user code for ZIO.die") {
        def subcall(): Unit =
          Unsafe.unsafe { implicit unsafe =>
            Runtime.default.unsafe.run(ZIO.die(new RuntimeException("boom"))).getOrThrowFiberFailure()
          }
        def call1(): Unit = subcall()

        assertStackTraceConsistency(() => call1())
      },
      test("captures full stack trace including user code for Exit.fail") {
        def subcall(): Unit =
          Unsafe.unsafe { implicit unsafe =>
            val exit = Runtime.default.unsafe.run(ZIO.fail(new RuntimeException("boom")).exit)
            exit.getOrThrowFiberFailure()
          }
        def call1(): Unit = subcall()

        assertStackTraceConsistency(() => call1())
      },
      test("captures full stack trace including user code for Exit.die") {
        def subcall(): Unit =
          Unsafe.unsafe { implicit unsafe =>
            val exit = Runtime.default.unsafe.run(ZIO.die(new RuntimeException("boom")).exit)
            exit.getOrThrowFiberFailure()
          }
        def call1(): Unit = subcall()

        assertStackTraceConsistency(() => call1())
      },
      test("captures full stack trace including user code for ZIO.interrupt") {
        def subcall(): Unit =
          Unsafe.unsafe { implicit unsafe =>
            Runtime.default.unsafe.run(ZIO.interrupt).getOrThrowFiberFailure()
          }
        def call1(): Unit = subcall()

        assertStackTraceConsistency(() => call1())
      }
    )

  private def assertStackTraceConsistency(call1: () => Unit) =
    for {
      fiberFailureTest <- ZIO
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

      stackTrace <- fiberFailureTest
      _          <- ZIO.log(s"Asserting stack trace:\n$stackTrace")
      _ <- ZIO.succeed {
             assertTrue(
               stackTrace.contains("call1") &&
                 stackTrace.contains("subcall") &&
                 stackTrace.contains("FiberFailureSpec")
             )
           }
    } yield ()
}
