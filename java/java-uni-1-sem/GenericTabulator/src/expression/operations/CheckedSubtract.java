package expression.operations;

import expression.CommonExpression;
import expression.exceptions.*;

public class CheckedSubtract extends Binary {
    public CheckedSubtract(CommonExpression a, CommonExpression b) {
        super(a, b);
    }

    public int applyOp(int x, int y) {
        if (x < 0 && y > 0) {
            int rem = Integer.MIN_VALUE - x;
            if (y > -rem) {
                throwException(x, y);
            }
        } else if (x >= 0 && y < 0) {
            int rem = Integer.MAX_VALUE - x;
            if (y < -rem) {
                throwException(x, y);
            }
        }
        return x - y;
    }

    private void throwException(int x, int y) {
        throw new IntegerOverflowException("Integer overflow during subtraction: " + x + " " + y);
    }
}

