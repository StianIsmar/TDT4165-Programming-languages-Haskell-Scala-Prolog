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
  var attempt = 0
  override def run: Unit = {

      def doTransaction() = {


        try {
          to.deposit.amount
          status = TransactionStatus.SUCCESS
        } catch {
          case exc: Exception =>
            from.deposit.amount
            attemt += 1
            if (attempt == allowedAttempts) {
              status = transactionStatus.FAILED
            }

        }
        catch
        {
          case exc: Exception =>
            attempt += 1
            if (attempt == allowedAttempts) {
              status = TransactionStatus.FAILED
            }
        }
      }
    this synchronized {
      if (status == TransactionStatus.PENDING){
        doTransaction
      }

      }
  }
}
