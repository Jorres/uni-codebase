package ru.ifmo.rain.tarasov.bank;

import java.rmi.*;
import java.rmi.registry.LocateRegistry;
import java.rmi.server.*;
import java.net.*;

public class Server {
    private final static int DEFAULT_PORT = 8888;
    private final static int DEFAULT_REGISTRY_PORT = 8889;

    private final int currentPort;
    private final int registryPort;

    /**
     * Launching server instance on specified ports.
     * @param port - port for server to listen on.
     * @param registryPort - port to start rmi registry on.
     * @throws RemoteException when failed to create registry.
     */
    public Server(final int port, final int registryPort) throws RemoteException {
        this.currentPort = port;
        this.registryPort = registryPort;
        LocateRegistry.createRegistry(registryPort);
    }

    /**
     * Rebinds a fresh instance of RemoteBank in the RMI registry
     * @throws RemoteException when failed to export object
     * @throws MalformedURLException never, url is hardcoded, just to convince java
     */
    public void rebindBank() throws RemoteException, MalformedURLException {
        final Bank bank = new RemoteBank(currentPort);
        UnicastRemoteObject.exportObject(bank, currentPort);
        Naming.rebind(Utilities.getRegistryAddress(registryPort, "bank"), bank);
    }

    /**
     * Main method to start server and bind a bank instance to regostry.
     * @param args unused
     */
    public static void main(String[] args) {
        try {
            Server server = new Server(DEFAULT_PORT, DEFAULT_REGISTRY_PORT);
            server.rebindBank();
        } catch (RemoteException e) {
            System.out.println("Failed to start server");
            e.printStackTrace();
        } catch (MalformedURLException e) {
            System.out.println("Malformed URL");
            e.printStackTrace();
        }
    }
}
