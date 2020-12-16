package ru.ifmo.rain.tarasov.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.*;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.stream.IntStream;

public class HelloUDPNonblockingClient implements HelloClient {
    private static final int TIMEOUT_IN_MS = 100;
    private static final long TIMEOUT_IN_NS = TimeUnit.MILLISECONDS.toNanos(TIMEOUT_IN_MS);

    private int requestsPerThread;
    private String requestPrefix;
    private SocketAddress addr;

    private Queue<RequestContainer> sentRequests;
    private List<Integer> currentActiveInThread;

    private static class RequestContainer {
        public RequestContainer(final long timestamp,
                                final int tag,
                                final int threadId,
                                final SelectionKey key) {
            this.tag = tag;
            this.timestamp = timestamp;
            this.threadId = threadId;
            this.key = key;
        }

        final long timestamp;
        final int tag;
        final int threadId;
        final SelectionKey key;
    }

    private static class DataContainer {
        DataContainer(final int threadId,
                      final int reqId,
                      final ByteBuffer recvBuffer,
                      final int tag) {
            this.threadId = threadId;
            this.reqId = reqId;
            this.recvBuffer = recvBuffer;
            this.tag = tag;
        }

        final int threadId;

        int reqId;
        ByteBuffer recvBuffer;
        int tag;
    }

    public static void main(final String[] args) {
        HelloUDPClient.mainWithFactory(args, HelloUDPNonblockingClient::new);
    }

    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        try {
            addr = new InetSocketAddress(InetAddress.getByName(host), port);
        } catch (UnknownHostException e) {
            System.out.println("Failed to locate address, aborting");
            return;
        }

        try (Selector selector = Selector.open()) {
            prepareSelectorForAccepting(selector, threads);

            requestsPerThread = requests;
            requestPrefix = prefix;

            sentRequests = new LinkedBlockingQueue<>();
            currentActiveInThread = new ArrayList<>(Collections.nCopies(threads, 0));

            enterMainEventLoop(selector);
        } catch (IOException e) {
            System.out.println("Failed to open selector, aborting...");
        }
    }

    private void prepareSelectorForAccepting(final Selector selector, final int threads) {
        ArrayList<DatagramChannel> channels = new ArrayList<>();

        IntStream.range(0, threads).forEach((i) -> {
            try {
                DatagramChannel channel = DatagramChannel.open();
                ByteBuffer recvBuffer = Utilities.createEmptyBuffer(channel.socket());
                channel.configureBlocking(false);
                DataContainer context = new DataContainer(i, /* reqId */ 0, recvBuffer, /* tag */ 0);
                channel.register(selector, SelectionKey.OP_WRITE, context);

                channels.add(channel);
            } catch (IOException ignored) {
                System.out.println("Failed to instantiate " + i + "'th channel, aborting...");
                channels.forEach(channel -> {
                    try {
                        channel.close();
                    } catch (IOException ignore) {

                    }
                });
            }
        });
    }

    private void enterMainEventLoop(final Selector selector) {
        while (selector.isOpen()) {
            try {
                selector.select(TIMEOUT_IN_MS);
                Set<SelectionKey> readyKeys = selector.selectedKeys();
                if (readyKeys.isEmpty()) {
                    if (sentRequests.isEmpty()) {
                        break;
                    }
                    drainQueue();
                } else {
                    for (final Iterator<SelectionKey> i = readyKeys.iterator(); i.hasNext();) {
                        final SelectionKey key = i.next();
                        if (key.isReadable()) {
                            readFromKey(key);
                        } else {
                            writeToKey(key);
                        }
                        i.remove();
                    }
                }
            } catch (IOException e) {
                System.out.println("IOException during select:");
                e.printStackTrace();
            }
        }
    }

    private void drainQueue() {
        long curTime = System.nanoTime();
        while (!sentRequests.isEmpty() && (curTime - sentRequests.peek().timestamp) > TIMEOUT_IN_NS) {
            RequestContainer curRequest = sentRequests.poll();
            // if this request was not really answered by anything <==> this request is active
            // otherwise it was already processed by smth, and needs not to be answered

            if (currentActiveInThread.get(curRequest.threadId) == curRequest.tag) {
                writeToKey(curRequest.key);
            }
        }
    }

    private void writeToKey(final SelectionKey key) {
        final DatagramChannel channel = (DatagramChannel)key.channel();
        final DataContainer context = (DataContainer)key.attachment();

        try {
            ByteBuffer message = Utilities.prepareNewMessage(requestPrefix, context.threadId, context.reqId);
            channel.send(message, addr);
            pushToQueue(context.threadId, key);
            key.interestOps(SelectionKey.OP_READ);
        } catch (IOException e) {
            System.out.println("Write of thread " + context.threadId + " failed");
        }
    }

    private void pushToQueue(final int threadId, final SelectionKey key) {
        sentRequests.add(new RequestContainer(System.nanoTime(), currentActiveInThread.get(threadId), threadId, key));
    }

    private void readFromKey(final SelectionKey key) {
        final DatagramChannel channel = (DatagramChannel) key.channel();
        final DataContainer context = (DataContainer) key.attachment();

        ByteBuffer to = context.recvBuffer;
        to.clear();

        try {
            channel.receive(to);
            to.flip();
            String message = Utilities.extractFromByteBuffer(to);
            System.out.println("Message is " + message);
            if (Utilities.isItForMe(message, context.threadId, context.reqId)) {
                context.reqId++;
            } else {
                System.out.println("Wrong response received for: " + context.threadId + " " + context.reqId + " " + message);
            }

            invalidateLastRequest(context.threadId);

            if (context.reqId != requestsPerThread) {
                key.interestOps(SelectionKey.OP_WRITE);
            } else {
                channel.close();
            }
        } catch (IOException e) {
            System.out.println("Read of thread " + context.threadId + " failed, aborting this thread...");
        }
    }

    private void invalidateLastRequest(final int threadId) {
        currentActiveInThread.set(threadId, currentActiveInThread.get(threadId) + 1);
    }
}
