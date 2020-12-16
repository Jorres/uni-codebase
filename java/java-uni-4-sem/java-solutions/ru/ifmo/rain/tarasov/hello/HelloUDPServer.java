package ru.ifmo.rain.tarasov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;
import info.kgeorgiy.java.advanced.hello.Util;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.IllegalFormatException;
import java.util.concurrent.*;
import java.util.function.Supplier;
import java.util.stream.IntStream;

public class HelloUDPServer implements HelloServer {
    public static final String SERVER_PREFIX = "Hello, ";
    private static final int SLEEP = 1000;

    private DatagramSocket socket;

    private ThreadPoolExecutor receivingPool;

    public static void main(final String[] args) {
        mainWithFactory(args, HelloUDPServer::new);
    }

    public static void mainWithFactory(final String[] args, Supplier<HelloServer> factory) {
        Utilities.checkArgumentsForNulls(args, 2, "Usage: HelloUDPServer port threads");

        try (HelloServer server = factory.get()) {
            server.start(
                    Integer.parseInt(args[0]),
                    Integer.parseInt(args[1]));
            while (!Thread.currentThread().isInterrupted()) {
                try {
                    Thread.sleep(SLEEP);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        } catch (final IllegalFormatException e) {
            System.out.println("Illegal format, check your integer input");
        }
    }

    public HelloUDPServer() {
        socket = null;
        receivingPool = null;
    }

    @Override
    public void start(final int port, final int threads) {
        try {
            socket = new DatagramSocket(port);
        } catch (final SocketException e) {
            System.out.println("Unable to open a socket on port " + port);
            return;
        }

        receivingPool = (ThreadPoolExecutor)Executors.newFixedThreadPool(threads);
        receivingPool.setRejectedExecutionHandler(new ThreadPoolExecutor.DiscardPolicy());

        IntStream.range(0, threads).forEach((i) -> receivingPool.submit(this::listenAndRespond));
    }

    private void listenAndRespond() {
        final DatagramPacket received = createEmptyPacket();
        try {
            final byte[] buffer = new byte[socket.getReceiveBufferSize()];

            while (!socket.isClosed()) {
                received.setData(buffer);
                socket.receive(received);

                final String response = Utilities.getDataFromPacket(received);

                final String message = prepareMessage(response);
                Utilities.setDataToPacket(received, message);
                socket.send(received);
            }
        } catch (final IOException e) {
            System.out.println("Failed to receive packet: " + e.getMessage());
        }
    }

    private String prepareMessage(final String source) {
        return SERVER_PREFIX + source;
    }

    @Override
    public void close() {
        socket.close();
        try {
            receivingPool.shutdown();
            receivingPool.awaitTermination(1, TimeUnit.MINUTES);
        } catch (InterruptedException ignored) {

        }
    }

    private DatagramPacket createEmptyPacket() {
        return new DatagramPacket(new byte[0], 0);
    }
}
