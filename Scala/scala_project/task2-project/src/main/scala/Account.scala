import akka.actor._
import exceptions._
import scala.collection.immutable.HashMap

case class TransactionRequest(toAccountNumber: String, amount: Double)

case class TransactionRequestReceipt(toAccountNumber: String,
                                     transactionId: String,
                                     transaction: Transaction)

case class BalanceRequest()

class Account(val accountId: String, val bankId: String, val initialBalance: Double = 0) extends Actor {

    private var transactions = HashMap[String, Transaction]()

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    def getFullAddress: String = {
        bankId + accountId
    }

    def getTransactions: List[Transaction] = {
        // Should return a list of all Transaction-objects stored in transactions
        transactions.values.toList
    }

    def allTransactionsCompleted: Boolean = {
        // Should return whether all Transaction-objects in transactions are completed
        val listofTrans: List[Transaction] = getTransactions
        val b: Boolean = true
        for (i<-listofTrans)
            if !(i.isCompleted) b = false
        b

    }

    def withdraw(amount: Double): Unit ={
        balance.synchronized{
            if (balance.amount - amount < 0) throw new NoSufficientFundsException
            if (amount <= 0) throw ned IllegalAmountException
              balance.amount -= amount
        }
    }
    def deposit(amount: Double): Unit = {
        balance.synchronized{
            if (amount <=0) throw new IllegalAmountException
            balance.amount += amount
        }
    }
    def getBalanceAmount: Double = {
        balance.amount

    }

    def sendTransactionToBank(t: Transaction): Unit = {
        // Should send a message containing t to the bank of this account
        var bankRef: ActorRef = BankManager.findBank(bankId)
        bankRef ! t

    }

    def transferTo(accountNumber: String, amount: Double): Transaction = {

        val t = new Transaction(from = getFullAddress, to = accountNumber, amount = amount)

        if (reserveTransaction(t)) {
            try {
                withdraw(amount)
                sendTransactionToBank(t)

            } catch {
                case _: NoSufficientFundsException | _: IllegalAmountException =>
                    t.status = TransactionStatus.FAILED
            }
        }

        t

    }

    def reserveTransaction(t: Transaction): Boolean = {
        if (!transactions.contains(t.id)) {
            transactions += (t.id -> t)
            return true
        }
        false
    }

    override def receive = {
        case IdentifyActor => sender ! this

        case TransactionRequestReceipt(to, transactionId, transaction) => {
            // Process receipt
            if (transactions.contains(transactionId)){ // den er i transactions
                var transac = transactions.get(transactionId) // get it from the hashmap
                var trans = transac.get //To get the actual Transaction, and
                // not Option[Transaction]
                trans.status = transaction.status
                if (!transaction.isSuccessful){
                    this.deposit(transaction.amount)
                }
            }
        }
        //Return current balance OK
        case BalanceRequest => sender ! balance.amount
        case t: Transaction => {
            // Handle incoming transaction
            ???
        }

        case msg => ???
    }


}
