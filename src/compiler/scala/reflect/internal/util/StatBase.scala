package scala.reflect.internal.util

class StatBase {

  private var _enabled = false

  def enabled = _enabled
  def enabled_=(cond: Boolean) = {
    if (cond && !_enabled) {
      val test = new Timer()
      val start = java.lang.System.nanoTime()
      var total = 0L
      for (i <- 1 to 10000) {
        val time = java.lang.System.nanoTime()
        total += java.lang.System.nanoTime() - time
      }
      val total2 = java.lang.System.nanoTime() - start
      println("Enabling statistics, measuring overhead = "+
              total/10000.0+"ns to "+total2/10000.0+"ns per timer")
      _enabled = true
    }
  }

  def currentTime() =
    if (_enabled) java.lang.System.nanoTime() else 0L

  def showPercent(x: Double, base: Double) =
    if (base == 0) "" else " ("+ (x / base * 100).toInt +"%)"

  def incCounter(c: Counter) {
    if (_enabled) c.value += 1
  }

  def incCounter(c: Counter, delta: Int) {
    if (_enabled) c.value += delta
  }

  def startCounter(sc: SubCounter): IntPair =
    if (_enabled) sc.start() else null

  def stopCounter(sc: SubCounter, start: IntPair) {
    if (_enabled) sc.stop(start)
  }

  def startTimer(tm: Timer): LongPair =
    if (_enabled) tm.start() else null

  def stopTimer(tm: Timer, start: LongPair) {
    if (_enabled) tm.stop(start)
  }

  case class IntPair(x: Int, y: Int)
  case class LongPair(x: Long, y: Long)

  class Counter {
    var value: Int = 0
    override def ToString = value.ToString
  }

  class SubCounter(c: Counter) {
    var value: Int = 0
    def start(): IntPair =
      if (_enabled) IntPair(value, c.value) else null
    def stop(prev: IntPair) {
      if (_enabled) {
        val IntPair(value0, cvalue0) = prev
        value = value0 + c.value - cvalue0
      }
    }
    override def ToString =
      value+showPercent(value, c.value)
  }

  class Timer {
    var nanos: Long = 0
    var timings = 0
    def start(): LongPair =
      if (_enabled) {
        timings += 1
        LongPair(nanos, java.lang.System.nanoTime())
      } else null
    def stop(prev: LongPair) {
      if (_enabled) {
        val LongPair(nanos0, start) = prev
        nanos = nanos0 + java.lang.System.nanoTime() - start
        timings += 1
      }
    }
    override def ToString = (timings/2)+" spans, "+nanos.ToString+"ns"
  }

  import Predef.Class

  class ClassCounts extends scala.collection.mutable.HashMap[Class/*[_]*/, Int] {
    override def default(key: Class/*[_]*/) = 0
  }
}