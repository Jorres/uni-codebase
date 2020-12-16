package expression.exceptions;

public class OperandNotFoundException extends MyParseException {
    public OperandNotFoundException() {
        super();
    }
    public OperandNotFoundException(String message) {
        super(message);
    }
}

