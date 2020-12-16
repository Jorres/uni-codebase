package info.kgeorgiy.java.advanced.hello;

import org.junit.Assert;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Random;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Stream;

public class Util {
    public static final Charset CHARSET;

    private Util() {
    }

    public static String getString(DatagramPacket packet) {
        return new String(packet.getData(), packet.getOffset(), packet.getLength(), CHARSET);
    }

    public static byte[] getBytes(String string) {
        return string.getBytes(CHARSET);
    }

    public static void setString(DatagramPacket packet, String string) {
        byte[] bytes = string.getBytes(CHARSET);
        packet.setData(bytes);
        packet.setLength(packet.getData().length);
    }

    public static DatagramPacket createPacket(DatagramSocket socket) throws SocketException {
        return new DatagramPacket(new byte[socket.getReceiveBufferSize()], socket.getReceiveBufferSize());
    }

    public static String request(String string, DatagramSocket socket, SocketAddress address) throws IOException {
        send(socket, string, address);
        return receive(socket);
    }

    public static String receive(DatagramSocket socket) throws IOException {
        DatagramPacket inPacket = createPacket(socket);
        socket.receive(inPacket);
        return getString(inPacket);
    }

    public static void send(DatagramSocket socket, String request, SocketAddress address) throws IOException {
        DatagramPacket outPacket = new DatagramPacket(new byte[0], 0);
        setString(outPacket, request);
        outPacket.setSocketAddress(address);
        socket.send(outPacket);
    }

    public static String response(String request) {
        return String.format("Hello, %s", request);
    }

    public static AtomicInteger[] server(String prefix, int treads, double p, DatagramSocket socket) {
        AtomicInteger[] expected = (AtomicInteger[])Stream.generate(AtomicInteger::new).limit((long)treads).toArray((x$0) -> {
            return new AtomicInteger[x$0];
        });
        (new Thread(() -> {
            Random random = new Random(4357204587045842850L);

            try {
                while(true) {
                    DatagramPacket packet = createPacket(socket);
                    socket.receive(packet);
                    String request = getString(packet);
                    String message = "Invalid or unexpected request " + request;
                    Assert.assertTrue(message, request.startsWith(prefix));
                    String[] parts = request.substring(prefix.length()).split("_");
                    Assert.assertEquals(message, 2L, (long)parts.length);

                    try {
                        int thread = Integer.parseInt(parts[0]);
                        int no = Integer.parseInt(parts[1]);
                        Assert.assertTrue(message, 0 <= thread && thread < expected.length);
                        Assert.assertEquals(message, (long)no, (long)expected[thread].get());
                        if (p >= random.nextDouble()) {
                            expected[thread].incrementAndGet();
                            setString(packet, response(request));
                            socket.send(packet);
                        }
                    } catch (NumberFormatException var12) {
                        throw new AssertionError(message);
                    }
                }
            } catch (IOException var13) {
                System.err.println(var13.getMessage());
            }
        })).start();
        return expected;
    }

    static void setMode(String test) {
    }

    static {
        CHARSET = StandardCharsets.UTF_8;
    }
}
