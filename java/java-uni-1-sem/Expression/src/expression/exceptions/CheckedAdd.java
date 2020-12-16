package expression.exceptions;

import expression.Binary;
import expression.CommonExpression;

public class CheckedAdd extends Binary {
    public CheckedAdd(CommonExpression a, CommonExpression b) {
        super(a, b);
    }

    public int applyOp(int x, int y) {
        if (x > 0 && y > 0) {
            int rem = Integer.MAX_VALUE - x;
            if (y > rem) {
                throwException(x, y);
            }
        } else if (x < 0 && y < 0) {
            int rem = Integer.MIN_VALUE - x;
            if (y < rem) {
                throwException(x, y);
            }
        }
        return x + y;
    }

    private void throwException(int x, int y) {
        throw new IntegerOverflowException("Integer overflow during addition: " + x + " " + y);
    }
}
