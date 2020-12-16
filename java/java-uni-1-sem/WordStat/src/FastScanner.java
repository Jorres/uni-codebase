import java.io.*;
import java.util.*;
import java.lang.*;

public class FastScanner {

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

    public FastScanner(InputStream inputStream) {
        reader = inputStream;
        finishConstruction();
    }

    // place for Encoding constructor

    // place for File constructor

    private void renewBuffer() { // clean
        try {
            bufferPos = 0;
            validCharsInBuffer = reader.read(buffer, 0, BUFFER_SIZE);
        } catch (IOException e) {
            System.out.println("Error occurred while reading buffer, further behaviour undefined.");
        }
    }

    public void close() {
        try {
            reader.close();
        } catch (IOException e) {
           System.out.println("Couldn't close inputStream, further behavior undefined.");
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

    private int transformFromUTF8(int a) {
        return (a & ((1 << 6) - 1));
    }

    private int getFirstOnesAmount(int val) {
        int ed = 0;
        for (int j = 7; j >= 0; j--) {
            if ((val & (1 << j)) == 0) {
                break;
            }
            ed++;
        }
        return ed;
    }

    private byte getNextByte() throws NoSuchElementException {
        if (!hasNextChar()) {
            throw new NoSuchElementException();
        }
        return buffer[bufferPos++];
    }

    public char nextChar() throws NoSuchElementException {
        byte first = getNextByte();

        int ans;
        if (first >= 0) {
            ans = first;
        } else {
            int totalBytes = getFirstOnesAmount(first);
            ans = (first & ((1 << (7 - totalBytes)) - 1));
            for (int i = 0; i < totalBytes - 1; i++) {
                byte last = getNextByte();
                ans = (ans << 6) | transformFromUTF8(last);
            }
        }

        return (char)ans;
    }

    public String nextLine() {
        if (!hasNextChar()) {
            throw new NoSuchElementException();
        }

        StringBuilder res = new StringBuilder();
        for (; bufferPos < validCharsInBuffer || hasNextChar();) {
            if (buffer[bufferPos] == lineSeparator.charAt(0)) {
                bufferPos++;
                if (lineSeparator.length() != 1 && hasNextChar()) {
                    bufferPos++;
                }
                break;
            }
            res.append(nextChar());
        }

        return res.toString();
    }
}
