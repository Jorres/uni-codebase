package expression.exceptions;

public class IntegerOverflowException extends RuntimeException {
    public IntegerOverflowException() {
        super();
    }
    public IntegerOverflowException(String message) {
        super(message);
    }
}
