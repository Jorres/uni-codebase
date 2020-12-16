package ru.ifmo.rain.tarasov.bank;

import java.util.HashMap;
import java.util.Map;

public class LocalPerson implements ILocalPerson {
    private final Map<String, Account> accounts;
    private final String name;
    private final String surname;
    private final String passportId;

    /**
     * Initializing constructor for LocalPerson
     * @param name person name
     * @param surname person surname
     * @param passportId person passportId
     */
    public LocalPerson(final String name, final String surname, final String passportId) {
        this.name = name;
        this.surname = surname;
        this.passportId = passportId;
        this.accounts = new HashMap<>();
    }

    /**
     * Name getter for LocalPerson
     * @return person name
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * Surname getter for LocalPerson
     * @return person surname
     */
    @Override
    public String getSurname() {
        return surname;
    }

    /**
     * Passport ID getter for LocalPerson
     * @return passport ID of the person
     */
    @Override
    public String getPassportId() {
        return passportId;
    }

    private Account neutralAccount(final String accountId) {
        return accounts.computeIfAbsent(accountId, LocalAccount::new);
    }

    /**
     * Amount getter of money on specified account
     * @param accountId amount of money to be taken from
     * @return amount of money on specified account
     */
    @Override
    public int getAmount(final String accountId) {
        return neutralAccount(accountId).getAmount();
    }

    /**
     * Atomic amount setter of money on specified account
     * @param accountId amount of money to be taken from
     * @param amount amount to be incremented on
     */
    @Override
    public int incrementAmount(final int amount, final String accountId) {
        Account account = neutralAccount(accountId);
        System.out.println("in local " + account.getAmount() + amount);
        account.setAmount(account.getAmount() + amount);
        return account.getAmount();
    }
}
