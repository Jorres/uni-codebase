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

    public FastScanner(FileInputStream inputStream) {
        reader = inputStream;
        finishConstruction();
    }

    public FastScanner(String s) {
        reader = new ByteArrayInputStream(s.getBytes());
    }

    private void renewBuffer() { // clean
        try {
            bufferPos = 0;
            validCharsInBuffer = reader.read(buffer, 0, BUFFER_SIZE);
        } catch (IOException e) {
            //System.out.println("Error occurred while reading buffer, further behaviour undefined.");
        }
    }

    public void close() {
        try {
            reader.close();
        } catch (IOException e) {
           // System.out.println("Couldn't close inputStream, further behavior undefined.");
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

    public String nextLine() {
        if (!hasNextChar()) {
            throw new NoSuchElementException();
        }

        StringBuilder res = new StringBuilder();
        for (; bufferPos < validCharsInBuffer || hasNextChar(); bufferPos++) {
            if (buffer[bufferPos] == lineSeparator.charAt(0)) {
                bufferPos++;
                if (lineSeparator.length() != 1 && hasNextChar()) {
                    bufferPos++;
                }
                break;
            }
            res.append((char)buffer[bufferPos]);
        }

        return res.toString();
    }
}
