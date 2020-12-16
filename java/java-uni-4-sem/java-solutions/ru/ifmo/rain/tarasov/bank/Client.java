package ru.ifmo.rain.tarasov.bank;

import java.net.MalformedURLException;
import java.rmi.Naming;
import java.rmi.NotBoundException;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;

public class Client {
    private final static int REGISTRY_PORT = 8889;

    public Client() {

    }

    public static void main(final String... args) {
        Utilities.assertClientArgumentsNonNull(args);

        final Bank bank;
        try {
            LocateRegistry.getRegistry();
            bank = (Bank) Naming.lookup(Utilities.getRegistryAddress(REGISTRY_PORT, "bank"));
        } catch (final NotBoundException e) {
            System.out.println("Bank is not bound");
            return;
        } catch (final MalformedURLException e) {
            System.out.println("Bank URL is invalid");
            return;
        } catch (final RemoteException e) {
            System.out.println("Failed to fetch bank");
            e.printStackTrace();
            return;
        }

        int change;
        try {
            change = Integer.parseInt(args[4]);
        } catch (NumberFormatException e) {
            System.out.println("Failed to parse change parameter, defaulting to 0");
            change = 0;
        }

        final RequestContainer container = new RequestContainer(args[0], args[1], args[2], args[3], change);

        executeRemoteRequest(container, bank);
    }

    public static void executeRemoteRequest(final RequestContainer container, final Bank bank) {
        try {
            IRemotePerson person = bank.getOrDefaultRemotePerson(container.name,
                                                                 container.surname,
                                                                 container.passportId);

            printCurrentStats(container, person.getAmount(container.accountId));
            final int result = person.incrementAmount(container.change, container.accountId);
            System.out.println("Money: " + result);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    public static void executeLocalRequest(final RequestContainer container, final Bank bank) {
        try {
            ILocalPerson person = bank.getOrDefaultLocalPerson(container.name,
                    container.surname,
                    container.passportId);

            printCurrentStats(container, person.getAmount(container.accountId));
            final int result = person.incrementAmount(container.change, container.accountId);
            System.out.println("Money: " + result);
        } catch (RemoteException e) {
            e.printStackTrace();
        }
    }

    private static void printCurrentStats(RequestContainer container, int curMoney) {
        System.out.println("Account id: " + container.accountId);
        System.out.println("Money: " + curMoney);
        System.out.println("Adding money");
    }
}
