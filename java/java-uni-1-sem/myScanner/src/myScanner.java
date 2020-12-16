import java.io.*;
import java.util.*;

class myScanner implements AutoCloseable {
    private final int BUFFER_SIZE = 4096;
    private InputStreamReader reader;
    private char[] buffer;
    private int pos;
    private int validCharsInBuffer;
    private String lineSeparator;
    private int lenOfLineSeparator;
    private boolean closed;

    public myScanner(String fileName) throws IOException {
        this(fileName, "UTF-8");
    }

    public myScanner(String fileName, String charset) throws IOException {
        this(new FileInputStream(fileName), charset);
    }

    public myScanner(InputStream input) {
        this(input, "UTF-8");
    }

    public myScanner(InputStream input, String charset) {
        try {
            reader = new InputStreamReader(input, charset);
        } catch (IOException IOExc) {
            throw new UncheckedIOException(IOExc);
        }
        buffer = new char[BUFFER_SIZE];
        closed = false;
        pos = 0;
        validCharsInBuffer = 0;
        lineSeparator = System.lineSeparator();
        lenOfLineSeparator = lineSeparator.length();
    }

    private void loadBuffer() {
        if (closed) {
            return;
        }
        pos = 0;
        try {
            for (validCharsInBuffer = 0; validCharsInBuffer < BUFFER_SIZE && (reader.ready() || validCharsInBuffer == 0); ++validCharsInBuffer) {
                int c = reader.read();
                if (c == -1) {
                    break;
                }
                buffer[validCharsInBuffer] = (char) c;
            }
        } catch (IOException IOExc) {
            throw new UncheckedIOException(IOExc);
        }
    }

    private boolean endOfLine(char c) {
        return c == '\n' || c == '\r';
    }

    public boolean hasNext() {
        if (closed) {
            throw new NullPointerException("Scanner is closed");
        }
        if (pos >= validCharsInBuffer) {
            loadBuffer();
        }
        return pos < validCharsInBuffer;
    }

    public String nextLine() {
        if (!((pos < validCharsInBuffer) || hasNext())) {
            throw new NoSuchElementException();
        }
        StringBuilder result = new StringBuilder();
        for (; ((pos < validCharsInBuffer) || hasNext()); ++pos) {
            if (buffer[pos] == lineSeparator.charAt(0)){
                if (lenOfLineSeparator == 1) {
                    ++pos;
                    break;
                } else {
                    if (pos < validCharsInBuffer - 1){
                        if (buffer[pos + 1] == lineSeparator.charAt(1)){
                            ++pos;
                            ++pos;
                            break;
                        }
                    } else {
                        ++pos;
                        if (((pos < validCharsInBuffer) || hasNext())){
                            if (buffer[0] == lineSeparator.charAt(1)){
                                ++pos;
                                break;
                            } else {
                                result.append(lineSeparator.charAt(0));
                            }
                        } else {
                            result.append(lineSeparator.charAt(0));
                            break;
                        }
                    }
                }
            }
            result.append(buffer[pos]);
        }
        return result.toString();
    }

    public int nextInt() {
        return nextInt(10);
    }

    private void skipSpaces() {
        while (((pos < validCharsInBuffer) || hasNext()) && (Character.isWhitespace(buffer[pos]) || endOfLine(buffer[pos]))) {
            ++pos;
        }
    }
    private void skipSequence() {
        while (((pos < validCharsInBuffer) || hasNext()) && !Character.isWhitespace(buffer[pos]) && !endOfLine(buffer[pos])) {
            ++pos;
        }
    }
    public int nextInt(int radix) {
        if (radix < Character.MIN_RADIX || radix > Character.MAX_RADIX) {
            throw new NumberFormatException("Invalid radix");
        }
        skipSpaces();
        if (!((pos < validCharsInBuffer) || hasNext())) {
            throw new NoSuchElementException();
        }
        boolean negative = false;
        if (buffer[pos] == '-') {
            negative = true;
            ++pos;
        }
        if (buffer[pos] == '+') {
            ++pos;
        }
        if (!((pos < validCharsInBuffer) || hasNext())) {
            throw new NumberFormatException("Not a valid number");
        }
        int result = 0;
        int lastCorrectNumForMultiply = Integer.MAX_VALUE / radix;
        for (; ((pos < validCharsInBuffer) || hasNext()) && !endOfLine(buffer[pos]) && !Character.isWhitespace(buffer[pos]); ++pos) {
            int digit;
            if ('0' <= buffer[pos] && buffer[pos] <= '9') {
                digit = buffer[pos] - '0';
            } else if ('A' <= buffer[pos] && buffer[pos] <= 'Z') {
                digit = buffer[pos] - 'A' + 10;
            } else {
                skipSequence();
                throw new NumberFormatException("Not a valid number");
            }
            if (digit >= radix) {
                skipSequence();
                throw new NumberFormatException("Not a valid number or invalid radix");
            }
            if (result > lastCorrectNumForMultiply){
                throw new ArithmeticException("Integer overflow");
            }
            result *= radix;
            if (Integer.MAX_VALUE - result < digit){
                throw new ArithmeticException("Integer overflow");
            }
            result += digit;
        }
        return negative ? -result : result;
    }

    public void close() {
        if (closed) {
            return;
        }
        try {
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
        closed = true;
        reader = null;
    }
}

