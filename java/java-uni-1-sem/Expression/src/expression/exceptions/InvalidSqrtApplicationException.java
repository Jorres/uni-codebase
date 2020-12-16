package expression.exceptions;

public class InvalidSqrtApplicationException extends RuntimeException {
    public InvalidSqrtApplicationException() {
        super();
    }
    public InvalidSqrtApplicationException(String message) {
        super(message);
    }
}
