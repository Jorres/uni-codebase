package expression.operations;

import expression.CommonExpression;
import expression.exceptions.*;

public class Abs extends Unary {
    public Abs(CommonExpression e) {
        super(e);
    }

    public int applyOp(int a) {
        if (a == Integer.MIN_VALUE) {
            throw new IntegerOverflowException("Taken absolute value from Integer.MIN_VALUE");
        }
        return (a < 0) ? -a : a;
    }
}

