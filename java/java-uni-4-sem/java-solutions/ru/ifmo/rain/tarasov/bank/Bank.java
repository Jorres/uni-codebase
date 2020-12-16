package ru.ifmo.rain.tarasov.bank;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface Bank extends Remote {
    /**
     * Returns local account by identifier.
     * @param passportId account id
     * @param name person first name
     * @param surname person surname
     * @return account with specified identifier or {@code null} if such account does not exists.
     */
    ILocalPerson getOrDefaultLocalPerson(String name, String surname, String passportId) throws RemoteException;

    /**
     * Returns remote account by identifier.
     * @param passportId account id
     * @param name person first name
     * @param surname person surname
     * @return account with specified identifier or {@code null} if such account does not exists.
     */
    IRemotePerson getOrDefaultRemotePerson(String name, String surname, String passportId) throws RemoteException;

    int getPeopleContainerSize() throws RemoteException;
}
