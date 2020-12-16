package ru.ifmo.rain.tarasov.hello;

import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.Socket;
import java.net.SocketException;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Utilities {
    private static final int DEFAULT_BUFFER_SIZE = 65536;
    private static final Pattern pattern = Pattern.compile("[\\D]*([\\d]+)[\\D]+([\\d]+)[\\D]*");
    private static final Charset charset = StandardCharsets.UTF_8;

    static void checkArgumentsForNulls(String[] args, final int requiredArgs, String usageMessage) {
        Objects.requireNonNull(args);
        if (args.length < 1 || args.length > requiredArgs) {
            System.out.println(usageMessage);
            return;
        }
        for (int i = 0; i < requiredArgs; i++) {
            Objects.requireNonNull(args[i], "args[" + i + "] must be non-null");
        }
    }

    static void setDataToPacket(final DatagramPacket packet, final String message) {
        packet.setData(message.getBytes(charset));
    }

    static String getDataFromPacket(final DatagramPacket packet) {
        return new String(packet.getData(),
                packet.getOffset(),
                packet.getLength(),
                charset);
    }

    static String extractFromByteBuffer(final ByteBuffer bbuffer) {
        return charset.decode(bbuffer).toString();
    }


    static ByteBuffer passStringToByteBuffer(final String msg) {
        return charset.encode(msg);
    }

    static ByteBuffer prepareNewMessage(final String prefix, final int threadNum, final int reqId) {
        return passStringToByteBuffer(prefix + threadNum + "_" + reqId);
    }

    static boolean isItForMe(final String response, final int thread, final int reqId) {
        Matcher m = pattern.matcher(response);
        return (m.matches()
                && m.group(1).equals(String.valueOf(thread))
                && m.group(2).equals(String.valueOf(reqId)));
    }

    static ByteBuffer createEmptyBuffer(final DatagramSocket socket) {
        int size = DEFAULT_BUFFER_SIZE;
        try {
            size = socket.getReceiveBufferSize();
        } catch (SocketException ignored) {
        }
        return ByteBuffer.wrap(new byte[size]);
    }
}
