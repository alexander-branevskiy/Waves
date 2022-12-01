package com.wavesplatform.lang.utils

import org.scalajs.dom.*

trait Logging {
  def trace(message: => String): Unit = println(message)
}
