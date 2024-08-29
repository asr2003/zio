/*
 * Copyright 2017-2024 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio

import zio.stacktracer.TracingImplicits.disableAutoTrace
import java.io.{PrintStream, PrintWriter}
import java.lang.System.arraycopy

/**
 * Represents a failure in a fiber. This could be caused by some non-
 * recoverable error, such as a defect or system error, by some typed error, or
 * by interruption (or combinations of all of the above).
 *
 * This class is used to wrap ZIO failures into something that can be thrown, to
 * better integrate with Scala exception handling.
 */
final case class FiberFailure(cause: Cause[Any]) extends Throwable(null, null, true, true) {
  override def getMessage: String = cause.unified.headOption.fold("<unknown>")(_.message)

  override def getStackTrace(): Array[StackTraceElement] = {
    implicit val trace: Trace = Trace.empty

    // Filter Java stack trace to remove internal ZIO methods
    val javaStackTrace = StackTrace.fromJava(FiberId.None, super.getStackTrace()).toJava.toArray

    val zioStackTrace = cause.unified.headOption.fold[Chunk[StackTraceElement]](Chunk.empty)(_.trace).toArray

    val combinedStackTrace = new Array[StackTraceElement](zioStackTrace.length + javaStackTrace.length)
    zioStackTrace.copyToArray(combinedStackTrace, 0)
    javaStackTrace.copyToArray(combinedStackTrace, zioStackTrace.length)

    combinedStackTrace
  }

  override def getCause(): Throwable =
    cause.find { case Cause.Die(throwable, _) => throwable }
      .orElse(cause.find { case Cause.Fail(value: Throwable, _) => value })
      .orNull

  def fillSuppressed()(implicit unsafe: Unsafe): Unit =
    if (getSuppressed().length == 0) {
      cause.unified.iterator.drop(1).foreach(unified => addSuppressed(unified.toThrowable))
    }

  override def toString: String = {
    val threadName       = Thread.currentThread().getName
    val stackTraceString = getStackTrace().mkString("\n\tat ", "\n\tat ", "")
    s"""Exception in thread "$threadName" ${cause.prettyPrint}"""
  }

  override def printStackTrace(s: PrintStream): Unit =
    s.println(this.toString)

  override def printStackTrace(s: PrintWriter): Unit =
    s.println(this.toString)

  override def printStackTrace(): Unit =
    java.lang.System.err.println(this.toString)
}
