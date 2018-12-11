import scala.concurrent.forkjoin.ForkJoinPool

class Bank(val allowedAttempts: Integer = 3) {

    private val uid: AtomicLong = new AtomicLong
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()
    private val executorContext = ???

    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        val t = new Transaction(
            transactionsQueue, processedTransactions, from,to,amount,allowedAttempts)
        transactionsQueue push(t)
        executorContext submit(new Runnable{
            def run(): Unit ={
                processTransactions
            }
        })
    }

    // Hint: use a counter! Alright. uid is an AtomicLong.
    def generateAccountId: Long = {
        uid.incrementAndGet
    }

    private def processTransactions: Unit = {
        val t: Transaction = transactionQueue.pop
        executorContext.submit(t)
        Thread sleep(80)
        if (t.synchronized {t.status == TransactionStatus.PENDING}){
            transationQueue.push(t)
            //thread sleep(50)
            executorContext.submit(new Runnable {
                def run(): Unit ={
                    processTransactions
                }
            })
        }
        else
            processedTransactions.push(t)
    }

    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
