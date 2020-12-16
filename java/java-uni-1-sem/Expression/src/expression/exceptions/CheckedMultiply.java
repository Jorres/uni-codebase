package expression.exceptions;

import expression.Binary;
import expression.CommonExpression;

public class CheckedMultiply extends Binary {
    public CheckedMultiply(CommonExpression a, CommonExpression b) {
        super(a, b);
    }

    public int applyOp(int x, int y) {
        int maxv = Integer.MAX_VALUE;
        int minv = Integer.MIN_VALUE;

        if (x > y) {
            int c = x;
            x = y;
            y = c;
        }
        if (x > 0) {
            x = -x;
            y = -y;
        }

        if (y > 0) {
            if (minv / y > x) {
                throwException(x, y);
            }
        } else if (y < 0) {
            if (maxv / y > x) {
                throwException(x, y);
            }
        }
        return x * y;
    }

    private void throwException(int x, int y) {
        throw new IntegerOverflowException("Integer overflow during multiplication: " + x + " " + y);
    }
}
