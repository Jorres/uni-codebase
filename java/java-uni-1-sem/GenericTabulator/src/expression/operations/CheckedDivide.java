package expression.operations;

import expression.CommonExpression;
import expression.exceptions.*;

public class CheckedDivide extends Binary {
    public CheckedDivide(CommonExpression a, CommonExpression b) {
        super(a, b);
    }

    public int applyOp(int x, int y) {
        if (y == 0) {
            throwDivisionByZeroException(x);
        }
        if (x == Integer.MIN_VALUE && y == -1) {
            throwOverflowException(x, y);
        }
        return x / y;
    }

    private void throwDivisionByZeroException(int x) {
        throw new DivisionByZeroException("Division by zero: " + x + " 0");
    }

    private void throwOverflowException(int x, int y) {
        throw new IntegerOverflowException("Integer overflow during division: " + x + " " + y);
    }
}
