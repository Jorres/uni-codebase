package ru.ifmo.rain.tarasov.bank;

import java.io.UncheckedIOException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

public class RemoteBank implements Bank {
    private final int port;
    private final ConcurrentMap<String, RemotePerson> proxies = new ConcurrentHashMap<>();

    public RemoteBank(final int port) {
        this.port = port;
    }

    /**
     * Atomic creation of person and it's proxy wrapper.
     * @param name person name
     * @param surname person surname
     * @param passportId person passportId
     * @return {@link IRemotePerson} via remote reference
     */
    public IRemotePerson atomicCreatePerson(String name, String surname, String passportId) throws RemoteException {
        System.out.println("Attempting to create a person " + name + " " + surname + " " + passportId);

        try {
            return proxies.computeIfAbsent(passportId, (newPassportId) -> {
                final RemotePerson proxy = new RemotePerson(name, surname, newPassportId);

                try {
                    UnicastRemoteObject.exportObject(proxy, port);
                } catch (RemoteException e) {
                    throw new UncheckedIOException(e);
                }

                return proxy;
            });
        } catch (UncheckedIOException e) {
            throw new RemoteException("Failed to export proxy", e.getCause());
        }
    }

    /**
     * Getter of {@link ILocalPerson}
     * @param name person first name
     * @param surname person surname
     * @param passportId account id
     * @return {@link ILocalPerson} via remote reference
     */
    public ILocalPerson getOrDefaultLocalPerson(final String name,
                                                             final String surname,
                                                             final String passportId) throws RemoteException  {
        System.out.println("Making snapshot of " + passportId);
        return atomicCreatePerson(name, surname, passportId).getCorrespondingLocal();
    }

    /**
     * Getter of {@link IRemotePerson}
     * @param name person first name
     * @param surname person surname
     * @param passportId account id
     * @return {@link IRemotePerson} via remote reference
     */
    public IRemotePerson getOrDefaultRemotePerson(final String name,
                                                               final String surname,
                                                               final String passportId) throws RemoteException  {
        System.out.println("Creating remote reference for " + passportId);
        return atomicCreatePerson(name, surname, passportId);
    }

    public int getPeopleContainerSize() {
        return proxies.size();
    }
}
