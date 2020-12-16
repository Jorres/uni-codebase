package ru.ifmo.rain.tarasov.bank;

public class RemoteAccount implements Account {
    private final String id;
    private volatile int amount;

    public RemoteAccount(final String id) {
        this.id = id;
        amount = 0;
    }

    public synchronized int getAmount() {
        //System.out.println("Getting amount of money for account " + id);
        return amount;
    }

    public synchronized void setAmount(final int amount) {
        //System.out.println("Setting amount of money for account " + id);
        this.amount = amount;
    }
}
