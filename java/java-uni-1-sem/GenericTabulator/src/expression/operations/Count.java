package expression.operations;

import expression.CommonExpression;

public class Count extends Unary {
    public Count(CommonExpression a) {
        super(a);
    }

    public int applyOp(int x) {
        int ans = 0;
        while (x != 0) {
            x = x & (x - 1);
            ans++;
        }
        return ans;
    }

    public int Tmp() {
        return 42;
    }
}

