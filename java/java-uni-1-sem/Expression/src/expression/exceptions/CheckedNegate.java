package expression.exceptions;

import expression.CommonExpression;
import expression.Unary;

public class CheckedNegate extends Unary {
    public CheckedNegate(CommonExpression e) {
        super(e);
    }
    public int applyOp(int a) {
        if (a == Integer.MIN_VALUE) {
            throw new IntegerOverflowException("Integer overflow: " + a);
        }
        return -a;
    }
}
