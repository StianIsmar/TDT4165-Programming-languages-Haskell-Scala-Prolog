import exceptions._


// Should be able to WITHDRAW + DEPOSIT. THREAD SAFE (Use val instead of var)
//Trow suitable exception!

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)
    val uid = bank.generateAccountId

    def withdraw(amount: Double): Unit = balance.synchronized {
        amount match {
            case amount if balance.amount - amount <0 => throw new NoSufficientFundsException()
            case amount if amount <= 0 => throw new IllegalAmountException()
            case _ => balance= balance - amount
        }
    }

    def deposit(amount: Double): Unit = balance.synchronized{
        amount match {
            if (amount <= 0) throw new IllegalAmountException()
            else balance += amount
        }
    }
    def getBalanceAmount: Double = this.balance

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}
