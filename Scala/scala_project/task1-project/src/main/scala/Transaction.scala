import exceptions._
import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

  //New queue
  private val localQueue: Queue[Transaction] = new Queue

    // Remove and return the first element from the queue

    def pop: Transaction = localQueue.synchronize{
      localQueue.dequeue() //Returns the top element in the queue!
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = localQueue.synchronized{
      localQueue.isEmpty
    }

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = localQueue.synchronized {
      localQueue.enqueue(t)
    }

    // Return the first element from the queue without removing it
    def peek: Transaction = {
      localQueue.synchronized {
        localQueue {0}
      }
    }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = {
      localQueue.synchronized{
        localQueue.iterator

      }
    }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING

  override def run: Unit = {

      def doTransaction() = {
          from withdraw amount
          to deposit amount
      }

      if (from.uid < to.uid) from synchronized {
          to synchronized {
            doTransaction
          }
      } else to synchronized {
          from synchronized {
            doTransaction
          }
      }

      // Extend this method to satisfy requirements.
      
    }
}
