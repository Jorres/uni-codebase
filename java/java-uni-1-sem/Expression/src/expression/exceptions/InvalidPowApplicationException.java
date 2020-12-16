package expression.exceptions;

public class InvalidPowApplicationException extends RuntimeException {
    public InvalidPowApplicationException () {
        super();
    }
    public InvalidPowApplicationException (String message) {
        super(message);
    }
}
