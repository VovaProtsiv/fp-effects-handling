package exercise

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.util.concurrent.LinkedBlockingQueue

// Hints:
// - LinkedBlockingQueue for tasks
// - Workers run forever
object ThreadPoolExercise {

  case class Task(id: Int) extends Runnable {
    override def run(): Unit = {
      println(s"Task id: $id is blocking on ${Thread.currentThread().getName}")
      Thread.sleep(3000)
      println(s"Task id: $id is running on ${Thread.currentThread().getName}")
    }
  }

  class FixedThreadPool(noThreads: Int) {
    val queue: LinkedBlockingQueue[Runnable] = new LinkedBlockingQueue[Runnable]
    val workers = (0 until noThreads).map(i=> Worker(s"Worker №: $i"))
    workers.foreach(_.start())
    case class Worker(name: String) extends Thread(name) {
      override def run(): Unit = {
        while (true) {
          queue.take().run()
        }
      }
    }

    def execute(runnable: Runnable): Unit = {
      queue.put(runnable)
    }
  }

  def main(args: Array[String]): Unit = {


    val fixedThreadPool = new FixedThreadPool(Runtime.getRuntime.availableProcessors())

    (0 to 50).foreach { i =>
      fixedThreadPool.execute(Task(i))
    }
  }
}


