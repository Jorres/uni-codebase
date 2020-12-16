package ru.ifmo.rain.tarasov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.IllegalFormatException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class HelloUDPClient implements HelloClient {

    private static final int WAIT_FOR_RECEIVE_IN_MILLISECONDS = 100;
    private static final int WAIT_EPS = 100;
    private static final int ERROR_MULTIPLIER = 10;
    private static final Pattern pattern = Pattern.compile("[\\D]*([\\d]+)[\\D]+([\\d]+)[\\D]*");

    ExecutorService sendingPool;

    public static void main(final String[] args) {
        mainWithFactory(args, HelloUDPClient::new);
    }

    public static void mainWithFactory(final String[] args, Supplier<HelloClient> factory) {
        Utilities.checkArgumentsForNulls(args, 5, "Usage: HelloUDPClient host port prefix threads requests");

        try {
            factory.get().run(args[0],
                    Integer.parseInt(args[1]),
                    args[2],
                    Integer.parseInt(args[3]),
                    Integer.parseInt(args[4]));
        } catch (final IllegalFormatException e) {
            System.out.println("Illegal format, check your integer input");
        }
    }

    public HelloUDPClient() {

    }

    /**
     * Handle to start sending requests.
     * @param host server host
     * @param port server port
     * @param prefix request prefix
     * @param threads number of request threads
     * @param requests number of requests per thread.
     */
    @Override
    public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
        System.out.println("Total requests: " + requests);

        try {
            final InetSocketAddress toSocket = new InetSocketAddress(InetAddress.getByName(host), port);

            sendingPool = Executors.newFixedThreadPool(threads);

            for (int i = 0; i < threads; i++) {
                final int threadNum = i;
                sendingPool.submit(() -> sendAllRequests(toSocket, prefix, threadNum, requests));
            }

            sendingPool.shutdown();

            try {
                sendingPool.awaitTermination(ERROR_MULTIPLIER
                                            * requests
                                            * (WAIT_FOR_RECEIVE_IN_MILLISECONDS + WAIT_EPS),
                                             TimeUnit.MILLISECONDS); // each query will be at most timeout + EPS
            } catch (final InterruptedException e) {
                // ignore
            }
        } catch (final UnknownHostException e) {
            System.out.println("Unable to resolve host " + host);
        }
    }

    private void sendAllRequests(final SocketAddress addr, final String prefix, final int threadNum, final int requests) {
        try (final DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(WAIT_FOR_RECEIVE_IN_MILLISECONDS);

            final byte[] buffer = new byte[socket.getReceiveBufferSize()];

            final DatagramPacket packet = new DatagramPacket(buffer, buffer.length, addr);

            packet.setSocketAddress(addr);

            for (int i = 0; i < requests; i++) {
                final String message = prefix + threadNum + "_" + i;

                while (!socket.isClosed() || Thread.currentThread().isInterrupted()) {
                    try {
                        packet.setData(message.getBytes(StandardCharsets.UTF_8));
                        socket.send(packet);

                        packet.setData(buffer);
                        socket.receive(packet);
                        final String response = Utilities.getDataFromPacket(packet);

                        System.out.println("Received: " + response);

                        if (isItForMe(response, threadNum, i)) {
                            break;
                        }
                    } catch (final IOException e) {
                        System.out.println("Timeout expired, resending packet...");;
                    }
                }
            }
        } catch (final SocketException e) {
            e.printStackTrace();
        }
    }

    private boolean isItForMe(final String response, final int thread, final int reqId) {
        Matcher m = pattern.matcher(response);
        return (m.matches()
                && m.group(1).equals(String.valueOf(thread))
                && m.group(2).equals(String.valueOf(reqId)));
    }
}
