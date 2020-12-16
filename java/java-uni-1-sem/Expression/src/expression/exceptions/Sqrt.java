package expression.exceptions;

import expression.CommonExpression;
import expression.Unary;

public class Sqrt extends Unary {
    public Sqrt(CommonExpression e) {
        super(e);
    }

    public int applyOp(int a) {
        if (a < 0) {
            throw new InvalidSqrtApplicationException("Taken square root out of " + a);
        }
        int L = 0, R = 46340; // sqrt out of 2^31 - 1
        while (R - L > 1) {
            int M = (L + R) / 2;
            if (M * M <= a) {
                L = M;
            } else {
                R = M;
            }
        }
        return L;
    }
}
