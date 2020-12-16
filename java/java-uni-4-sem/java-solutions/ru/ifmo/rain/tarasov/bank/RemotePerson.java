package ru.ifmo.rain.tarasov.bank;

import java.rmi.RemoteException;

public class RemotePerson implements IRemotePerson {
    private final LocalPerson correspondingLocal;

    public RemotePerson(final String name, final String surname, final String passportId) {
        this.correspondingLocal = new LocalPerson(name, surname, passportId);
    }

    /**
     * The only constructor for RemotePerson
     * @param correspondingLocal instance of {@link LocalPerson} with corresponding data
     */
    public RemotePerson(final LocalPerson correspondingLocal) {
        this.correspondingLocal = correspondingLocal;
    }

    /**
     * Name getter for RemotePerson
     * @return name of the person
     */
    @Override
    public synchronized String getName() {
        return correspondingLocal.getName();
    }

    /**
     * Surname getter for RemotePerson
     * @return surname of the person
     */
    @Override
    public synchronized String getSurname() {
        return correspondingLocal.getSurname();
    }

    /**
     * Passport ID for RemotePerson
     * @return passport ID of the person
     */
    @Override
    public synchronized String getPassportId() {
        return correspondingLocal.getPassportId();
    }

    /**
     * Amount getter for specified account
     * @param accountId account from which amount is taken
     * @return amount of money on the specified account
     */
    @Override
    public synchronized int getAmount(final String accountId) {
        return correspondingLocal.getAmount(accountId);
    }

    /**
     * Atomic increment to the amount of money
     * @param change value to be incremented with
     * @param accountId account to be incremented
     */
    @Override
    public synchronized int incrementAmount(final int change, final String accountId) {
        return correspondingLocal.incrementAmount(change, accountId);
    }

    @Override
    public synchronized LocalPerson getCorrespondingLocal() {
        return correspondingLocal;
    }
}
