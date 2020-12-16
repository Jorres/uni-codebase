package expression.exceptions;

public class OperationNotFoundException extends MyParseException {
    public OperationNotFoundException() {
        super();
    }
    public OperationNotFoundException(String message) {
        super(message);
    }
}

