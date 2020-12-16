package expression.operations;

import expression.CommonExpression;

public class High extends Unary {
    public High(CommonExpression e) {
        super(e);
    }
    public int applyOp(int a) {
        return Integer.highestOneBit(a);
    }
}
