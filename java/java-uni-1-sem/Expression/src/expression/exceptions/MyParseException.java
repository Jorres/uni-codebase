package expression.exceptions;

public class MyParseException extends RuntimeException {
    public MyParseException() {
        super();
    }
    public MyParseException(String message) {
        super(message);
    }
}
