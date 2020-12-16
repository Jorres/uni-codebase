package ru.ifmo.rain.tarasov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface IRemotePerson extends Remote {
    /**
     * Name getter for RemotePerson
     * @return name of the person
     */
    String getName() throws RemoteException;

    /**
     * Surname getter for RemotePerson
     * @return surname of the person
     */
    String getSurname() throws RemoteException;

    /**
     * Passport ID for RemotePerson
     * @return passport ID of the person
     */
    String getPassportId() throws RemoteException;

    /**
     * Amount getter for specified account
     * @param accountId account from which amount is taken
     * @return amount of money on the specified account
     */
    int getAmount(final String accountId) throws RemoteException;

    /**
     * Atomic increment to the amount of money
     * @param change value to be incremented with
     * @param accountId account to be incremented
     * @return amount of money after increment
     */
    int incrementAmount(final int change, final String accountId) throws RemoteException;

    LocalPerson getCorrespondingLocal() throws RemoteException;
}
