package expression.exceptions;

import expression.CommonExpression;
import expression.Unary;

public class Log2 extends Unary {

    public Log2(CommonExpression e) {
        super(e);
    }

    public int applyOp(int a) {
        if (a <= 0) {
            throw new InvalidLogApplicationException("Log of " + a + " can not be counted.");
        }

        int L = -1, R = 31;

        while (R - L > 1) {
            int M = (L + R) / 2;

            int ans = 1;
            for (int i = 0; i < M; i++) {
                ans *= 2;
            }

            if (ans <= a) {
                L = M;
            } else {
                R = M;
            }
        }

        return L;
    }
}

