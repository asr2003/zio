package zio

import zio.test._
import java.io.{ByteArrayOutputStream, PrintStream}

object FiberFailureSpec extends ZIOBaseSpec {

  def spec =
    suite("FiberFailureSpec")(
      //   test("captures full stack trace including user code for ZIO.fail with String") {
      //     def subcall(): Unit =
      //       Unsafe.unsafe { implicit unsafe =>
      //         Runtime.default.unsafe.run(ZIO.fail("boom")).getOrThrowFiberFailure()
      //       }
      //     def call1(): Unit = subcall()

      //     assertStackTraceConsistency(() => call1())
      //   },
      //   test("captures full stack trace including user code for ZIO.fail with Throwable") {
      //     def subcall(): Unit =
      //       Unsafe.unsafe { implicit unsafe =>
      //         Runtime.default.unsafe.run(ZIO.fail(new RuntimeException("boom"))).getOrThrowFiberFailure()
      //       }
      //     def call1(): Unit = subcall()

      //     assertStackTraceConsistency(() => call1())
      //   },
      //   test("captures full stack trace including user code for ZIO.die") {
      //     def subcall(): Unit =
      //       Unsafe.unsafe { implicit unsafe =>
      //         Runtime.default.unsafe.run(ZIO.die(new RuntimeException("boom"))).getOrThrowFiberFailure()
      //       }
      //     def call1(): Unit = subcall()

      //     assertStackTraceConsistency(() => call1())
      //   },
      //   test("captures full stack trace including user code for Exit.fail") {
      //     def subcall(): Unit =
      //       Unsafe.unsafe { implicit unsafe =>
      //         val exit = Runtime.default.unsafe.run(ZIO.fail(new RuntimeException("boom")).exit)
      //         exit.getOrThrowFiberFailure()
      //       }
      //     def call1(): Unit = subcall()

      //     assertStackTraceConsistency(() => call1())
      //   },
      //   test("captures full stack trace including user code for Exit.die") {
      //     def subcall(): Unit =
      //       Unsafe.unsafe { implicit unsafe =>
      //         val exit = Runtime.default.unsafe.run(ZIO.die(new RuntimeException("boom")).exit)
      //         exit.getOrThrowFiberFailure()
      //       }
      //     def call1(): Unit = subcall()

      //     assertStackTraceConsistency(() => call1())
      //   },
      //   test("captures full stack trace including user code for ZIO.interrupt") {
      //     def subcall(): Unit =
      //       Unsafe.unsafe { implicit unsafe =>
      //         Runtime.default.unsafe.run(ZIO.interrupt).getOrThrowFiberFailure()
      //       }
      //     def call1(): Unit = subcall()

      //     assertStackTraceConsistency(() => call1())
      //   }
      // )

      test("captures the expected stack trace structure and ensures consistency") {
        def subcall(): Unit =
          Unsafe.unsafe { implicit unsafe =>
            Runtime.default.unsafe.run(ZIO.fail("boom")).getOrThrowFiberFailure()
          }
        def call1(): Unit = subcall()

        assertStackTraceConsistency(() => call1())
      }
    )

  private def cleanStackTrace(stackTrace: String): String =
    stackTrace
      .replaceAll("""\(.+?:\d+\)""", "(line)")
      .replaceAll("""\/.*\/""", "")

  private def assertStackTraceConsistency(call1: () => Unit) =
    for {
      fiberFailureTest <- ZIO
                            .attempt(call1())
                            .catchAll { case fiberFailure: FiberFailure =>
                              val stackTrace     = cleanStackTrace(fiberFailure.getStackTrace.mkString("\n"))
                              val toStringOutput = cleanStackTrace(fiberFailure.toString)
                              val printStackTraceOutput = {
                                val stream = new ByteArrayOutputStream()
                                fiberFailure.printStackTrace(new PrintStream(stream))
                                cleanStackTrace(stream.toString)
                              }
                              ZIO.succeed((stackTrace, toStringOutput, printStackTraceOutput))
                            }

      (stackTrace: String, toStringOutput: String, printStackTraceOutput: String) <- fiberFailureTest
      _ <- ZIO.succeed {
             val expectedStackTrace = """zio.FiberSpec.spec.subcall(FiberSpec.scala:line)
                                         java.base/java.lang.Thread.getStackTrace(Thread.java:line)
                                         zio.FiberFailure.<init>(FiberFailure.scala:line)
                                         zio.FiberFailure$.apply(FiberFailure.scala:line)
                                         zio.Exit.getOrThrowFiberFailure(ZIO.scala:line)
                                         zio.FiberSpec$.$anonfun$spec$134(FiberSpec.scala:line)
                                         zio.FiberRuntime.runLoop(FiberRuntime.scala:line)"""

             assertTrue(
               stackTrace.contains("zio.FiberSpec.spec.subcall"),
               stackTrace.contains("zio.FiberFailure.<init>"),
               stackTrace.contains("zio.FiberRuntime.runLoop"),
               stackTrace == expectedStackTrace.trim,
               toStringOutput.contains(stackTrace),
               printStackTraceOutput.contains(stackTrace)
             )
           }
    } yield ()
}
