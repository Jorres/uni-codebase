package expression.operations;

import expression.CommonExpression;
import expression.exceptions.*;

public class Pow2 extends Unary {
    public Pow2(CommonExpression e) {
        super(e);
    }
    public int applyOp(int a) {

        if (a < 0) {
            throw new InvalidPowApplicationException("Power of 2 by " + a + "can not be counted in integer value.");
        }

        int ans = 1;
        for (int i = 0; i < a; i++) {
            if (ans <= Integer.MAX_VALUE / 2) {
                ans *= 2;
            } else {
                throw new IntegerOverflowException("Integer overflow: 2 in power " + a);
            }
        }
        return ans;
    }
}

