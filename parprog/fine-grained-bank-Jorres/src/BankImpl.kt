import java.util.concurrent.locks.ReentrantLock
import kotlin.concurrent.withLock

/**
 * Bank implementation.
 *
 * @author Egor Tarasov
 */
class BankImpl(n: Int) : Bank {
    private val accounts: Array<Account> = Array(n) { Account() }

    override val numberOfAccounts: Int
        get() = accounts.size

    override fun getAmount(index: Int): Long {
            return accounts[index].amount;
    }

    // TODO why the hell not to maintain a single variable that holds totalAmount????

    override val totalAmount: Long
        get() {
            accounts.forEach { account -> account.lock.lock() }
            var ans = 0L
            accounts.forEach { account ->
                ans += account.amount
            }
            accounts.forEach { account -> account.lock.unlock() }
            return ans
        }

    override fun deposit(index: Int, amount: Long): Long {
        accounts[index].lock.withLock {
            require(amount > 0) { "Invalid amount: $amount" }
            val account = accounts[index]
            check(!(amount > Bank.MAX_AMOUNT || account.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }
            account.amount += amount
            return account.amount
        }
    }

    override fun withdraw(index: Int, amount: Long): Long {
        accounts[index].lock.withLock {
            require(amount > 0) { "Invalid amount: $amount" }
            val account = accounts[index]
            check(account.amount - amount >= 0) { "Underflow" }
            account.amount -= amount
            return account.amount
        }
    }

    override fun transfer(fromIndex: Int, toIndex: Int, amount: Long) {
        var leftId = fromIndex
        var rightId = toIndex
        if (fromIndex > toIndex) {
            val tmp = leftId
            leftId = rightId
            rightId = tmp
        }
        val mFirst = accounts[leftId].lock
        val mSecond = accounts[rightId].lock
        mFirst.withLock {
            mSecond.withLock {
                require(amount > 0) { "Invalid amount: $amount" }
                require(fromIndex != toIndex) { "fromIndex == toIndex" }
                val from = accounts[fromIndex]
                val to = accounts[toIndex]
                check(amount <= from.amount) { "Underflow" }
                check(!(amount > Bank.MAX_AMOUNT || to.amount + amount > Bank.MAX_AMOUNT)) { "Overflow" }
                from.amount -= amount
                to.amount += amount
            }
        }
    }

    /**
     * Private account data structure.
     */
    class Account {
        /**
         * Amount of funds in this account.
         */
        var lock: ReentrantLock = ReentrantLock()
        var amount: Long = 0
    }
}
