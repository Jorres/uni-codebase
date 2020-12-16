package ru.ifmo.rain.tarasov.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.Iterator;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.IntStream;

public class HelloUDPNonblockingServer implements HelloServer {
    private static final int MAXIMUM_TASKS_AT_ONCE = 1000;
    private DatagramChannel channel;
    private ThreadPoolExecutor receivingPool;
    private Selector selector;

    private Queue<ByteBuffer> freeBuffers;
    private Queue<ResponseContainer> results;

    private ExecutorService listener;

    private final AtomicBoolean wakeUpTerminator;

    private static class ResponseContainer {
        public ResponseContainer(final ByteBuffer message, final SocketAddress addr) {
            this.message = message;
            this.addr = addr;
        }

        private final ByteBuffer message;
        private final SocketAddress addr;
    }

    public static void main(final String[] args) {
        HelloUDPServer.mainWithFactory(args, HelloUDPNonblockingServer::new);
    }

    public HelloUDPNonblockingServer() {
        wakeUpTerminator = new AtomicBoolean(true);
    }

    /**
     * Start Nonblocking server on given port and with a given amount of working threads.
     * @param port server port.
     * @param threads number of working threads.
     */
    @Override
    public void start(final int port, final int threads) {
        try {
            channel = DatagramChannel.open();
            channel.configureBlocking(false);
            channel.socket().bind(new InetSocketAddress(port));
        } catch (final SocketException e) {
            System.out.println("Unable to open a socket on port " + port);
            return;
        } catch (IOException e) {
            System.out.println("Failed to create a channel, aborting...");
            return;
        }

        try {
            selector = Selector.open();
        } catch (IOException e) {
            System.out.println("Failed to open selector, aborting...");
            return;
        }

        try {
            channel.register(selector, SelectionKey.OP_READ);
        } catch (ClosedChannelException ignored) {
            // channel can not be closed by now
        }

        receivingPool = new ThreadPoolExecutor(threads, threads,
                0, TimeUnit.SECONDS,
                new ArrayBlockingQueue<>(MAXIMUM_TASKS_AT_ONCE), new ThreadPoolExecutor.DiscardPolicy());

        results = new ConcurrentLinkedQueue<>();
        listener = Executors.newSingleThreadExecutor();
        freeBuffers = new ArrayBlockingQueue<>(MAXIMUM_TASKS_AT_ONCE);

        IntStream.range(0, MAXIMUM_TASKS_AT_ONCE).forEach(i -> freeBuffers.add(Utilities.createEmptyBuffer(channel.socket())));
        listener.submit(this::listen);
    }

    private void work(final ByteBuffer buf, final SocketAddress addr) {
        buf.flip();
        String request = Utilities.extractFromByteBuffer(buf);
        String response = HelloUDPServer.SERVER_PREFIX + request;
        buf.clear();
        freeBuffers.add(buf);
        synchronized (wakeUpTerminator) {
            results.add(new ResponseContainer(Utilities.passStringToByteBuffer(response), addr));
            if (wakeUpTerminator.get()) {
                selector.wakeup();
            }
        }
    }

    private void listen() {
        while (channel.isOpen()) {
            try {
                selector.select();
                final Set<SelectionKey> readyKeys = selector.selectedKeys();
                if (readyKeys.isEmpty()) {
                    final SelectionKey channelKey = selector.keys().iterator().next();
                    synchronized (wakeUpTerminator) {
                        channelKey.interestOps(SelectionKey.OP_READ | SelectionKey.OP_WRITE);
                        wakeUpTerminator.set(false);
                    }
                } else {
                    final Iterator<SelectionKey> i = readyKeys.iterator();
                    final SelectionKey key = i.next();
                    if (key.isReadable()) {
                        acceptNewTask(key);
                    }
                    if (key.isWritable()) {
                        respondWithResult(key);
                    }
                    i.remove();
                }
            } catch (IOException e) {
                System.out.println("IOException during select: ");
                e.printStackTrace();
            }
        }
    }

    private void acceptNewTask(final SelectionKey key) {
        final DatagramChannel channel = (DatagramChannel) key.channel();

        ByteBuffer buf = freeBuffers.poll();
        if (buf != null) {
            try {
                SocketAddress addr = channel.receive(buf);
                receivingPool.submit(() -> work(buf, addr));
            } catch (IOException ignored) {
                // will exit in main event loop
            }
        } else {
            key.interestOps(SelectionKey.OP_WRITE);
        }
    }

    private void respondWithResult(final SelectionKey key) {
        synchronized (wakeUpTerminator) {
            key.interestOps(SelectionKey.OP_READ);
            wakeUpTerminator.set(true);

            boolean ableToSend =    true;
            while (!results.isEmpty() && ableToSend) {
                ResponseContainer response = results.poll();
                try {
                    ableToSend = channel.send(response.message, response.addr) != 0;
                } catch (IOException e) {
                    System.out.println("Failure during send to socket: " + e.getMessage());
                    ableToSend = false;
                }
            }
        }
    }

    /**
     * Closes the server and frees all allocated resources.
     */
    @Override
    public void close() {
        if (selector == null) {
            return;
        }

        try {
            channel.close();
        } catch (IOException e) {
            System.out.println("Channel closing failed");
        }

        try {
            selector.close();
        } catch (IOException e) {
            System.out.println("Selector closing failed");
        }

        try {
            receivingPool.shutdown();
            receivingPool.awaitTermination(receivingPool.getActiveCount() * 10, TimeUnit.SECONDS);
        } catch (InterruptedException ignore) {
        }

        try {
            listener.shutdown();
            listener.awaitTermination(1, TimeUnit.SECONDS);
        } catch (InterruptedException ignore) {
        }
    }
}