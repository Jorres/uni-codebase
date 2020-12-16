package md2html;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.lang.*;

public class FastScanner implements AutoCloseable {
    private final int BUFFER_SIZE = 4096;
    private int validCharsInBuffer;
    private int bufferPos;
    private InputStream reader;
    private byte[] buffer;
    private String lineSeparator;

    private void finishConstruction() {
        buffer = new byte[BUFFER_SIZE];
        lineSeparator = System.lineSeparator();
        renewBuffer();
    }

    private byte nextByte() {
        if (!hasNextChar()) {
            throw new NoSuchElementException();
        }

        return buffer[bufferPos++];
    }

    public char next() throws IOException {
        byte b = nextByte();
        if (b >= 0)
            return (char)b;
        int cnt = 3;
        while ((b & (1 << (8 - cnt))) != 0)
            ++cnt;
        int ans = b & ((1 << (8 - cnt)) - 1);
        cnt -= 2;
        for (int i = 0; i < cnt; ++i)
            ans = (ans << 6) | (nextByte() & ((1 << 6) - 1));
        return (char)ans;
    }

    public void close() {
        try {
            reader.close();
        } catch (IOException e) {
            System.out.println("Unable to close reader.");
        }
    }

    public FastScanner(InputStream inputStream) {
        reader = inputStream;
        finishConstruction();
    }

    // place for Encoding constructor

    public FastScanner(FileInputStream inputStream) {
        reader = inputStream;
        finishConstruction();
    }

    public FastScanner(String s) {
        reader = new ByteArrayInputStream(s.getBytes(StandardCharsets.UTF_8));
        finishConstruction();
    }

    private void renewBuffer() { // clean
        try {
            bufferPos = 0;
            validCharsInBuffer = reader.read(buffer, 0, BUFFER_SIZE);
        } catch (IOException e) {
            //System.out.println("Error occurred while reading buffer, further behaviour undefined.");
        }
    }

    private void skipWhitespaces() {
        while (hasNextChar() && Character.isWhitespace(buffer[bufferPos])) {
            bufferPos++;
        }
    }

    public boolean hasNextChar() {
        if (bufferPos >= validCharsInBuffer) {
            renewBuffer();
        }
        return bufferPos < validCharsInBuffer;
    }

    public char nextChar() {
        if (!hasNextChar()) {
            throw new NoSuchElementException();
        }

        return (char)buffer[bufferPos++];
    }

    public char testChar() {
        if (!hasNextChar()) {
            throw new NoSuchElementException();
        }
        return (char)buffer[bufferPos];
    }

    public String nextLine() throws IOException {
        if (!hasNextChar()) {
            throw new NoSuchElementException();
        }

        StringBuilder res = new StringBuilder();

        for (; bufferPos < validCharsInBuffer || hasNextChar();) {
            char c = next();
            if (c == lineSeparator.charAt(0)) {
                if (lineSeparator.length() != 1 && hasNextChar()) {
                    char cn = next();
                }
                break;
            }
            res.append(c);
        }

        return res.toString();
    }
}
