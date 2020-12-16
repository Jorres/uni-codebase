package expression.exceptions;

public class InvalidLogApplicationException extends RuntimeException {
    public InvalidLogApplicationException () {
        super();
    }

    public InvalidLogApplicationException (String message) {
        super(message);
    }
}
