package expression.exceptions;

public class UnknownTokenException extends MyParseException {
    public UnknownTokenException() {
        super();
    }
    public UnknownTokenException(String message) {
        super(message);
    }
}
