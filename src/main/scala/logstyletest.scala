import scala.collection.mutable.Queue

// A thread pool is a group of a limited number of threads that are used to
// execute tasks.
private object ThreadPool { var ID = 0 }

// Creates a new ThreadPool.
// @param numThreads The number of threads in the pool.
class ThreadPool(numThreads: Int) extends
  ThreadGroup("ThreadPool-" + ThreadPool.ID) {

  private var ID = 0
  private var isAlive = true
  private val taskQueue = new Queue[Runnable]

  ThreadPool.ID += 1
  setDaemon(true)
  for(i <- 0 until numThreads) new PooledThread start

  // Closes this ThreadPool and returns immediately. All threads are stopped,
  // and any waiting tasks are not executed. Once a ThreadPool is closed, no
  // more tasks can be run on this ThreadPool.
  def close = synchronized {
    if(isAlive) { isAlive = false; taskQueue.clear; interrupt }
  }

  protected def getTask: Option[Runnable] = synchronized {
    while(taskQueue.size == 0) {
      if(!isAlive) return None
      wait
    }
    Some(taskQueue dequeue)
  }

  // Closes this ThreadPool and waits for all running threads to finish. Any
  // waiting tasks are executed.
  def join {
    synchronized { isAlive = false; notifyAll }

    // wait for all threads to finish
    val threads = new Array[Thread](activeCount)
    for(i <- 0 until enumerate(threads))
      try { threads(i) join }
      catch { case e: InterruptedException => }
  }

  // Requests a new task to run. This method returns immediately, and the task
  // executes on the next available idle thread in this ThreadPool.Tasks start
  // execution in the order they are received.
  // @param task The task to run. If null, no action is taken.
  // @throws IllegalStateException if this ThreadPool is already closed.
  def runTask(task: Runnable) = synchronized {
    if(!isAlive) throw new IllegalStateException
    if(task != null) { taskQueue += task; notify }
  }

  // Signals that a PooledThread has started. This method does nothing by
  // default subclasses should override to do any thread-specific startup tasks.
  protected def threadStarted {}

  // Signals that a PooledThread has stopped. This method does nothing by
  // default subclasses should override to do any thread-specific cleanup tasks.
  protected def threadStopped {}

  private class PooledThread extends
    Thread(ThreadPool.this, "PooledThread-" + ID) {
    ID += 1
    override def run {
      // signal that this thread has started
      threadStarted

      while(!isInterrupted) {
        // get a task to run
        var task: Option[Runnable] =
          try { getTask }
          catch { case e: InterruptedException => None }
        // if getTask() returned null or was interrupted, close this thread
        // by returning.
        if(!task.isDefined) return
        // run the task, and eat any exceptions it throws
        try { task.get run }
        catch { case t: Throwable => uncaughtException(this, t) }
      }
    }
  }
}

class testJustIf extends Runnable {
	override def run() {
		if (false)
			println("never getting here")
	}
}

import org.apache.log4j.Logger;


trait LogHelper {
    val loggerName = this.getClass.getName
    lazy val logger = Logger.getLogger(loggerName)

	def debug(msg: => String) {
		if (false) //just to make it the same test
			println("never getting here")
	}	
}

class testNewLogStype extends Runnable with LogHelper {
	override def run() {
		debug("test" + "me")
	}
}

object logstyletest {
	def main(args: Array[String]) = {
		
		def justIfTest(pool: Int, fini: Int) = {
			var tp = new ThreadPool(pool) 
			var fini = args(1).toInt

			var startTime = System.currentTimeMillis();
			println("Start Just If - " + startTime)

			for (i <- 0 until fini) {
				val t = new testJustIf()
				tp.runTask(t)
			}

			tp.join

			var endTime = System.currentTimeMillis();
			println("Total Just If Time - " + (endTime-startTime))			
		}
		
		def newStyleTest(pool: Int, fini: Int) = {
			var tp = new ThreadPool(pool) 		
			var startTime = System.currentTimeMillis();
			println("Start LogStyle - " + startTime)

			for (i <- 0 until fini) {
				val t = new testNewLogStype()
				tp.runTask(t)
			}

			tp.join
			var endTime = System.currentTimeMillis();
			println("Total New Log Style - " + (endTime-startTime))			
		}

		for (j <- 0 until args(2).toInt) {
			justIfTest(args(0).toInt,args(1).toInt)
			newStyleTest(args(0).toInt,args(1).toInt)
		}					
		
		System.exit(0)
	}
}