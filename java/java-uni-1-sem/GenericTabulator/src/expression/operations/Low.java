package expression.operations;

import expression.CommonExpression;

public class Low extends Unary {
    public Low(CommonExpression e) {
        super(e);
    }
    public int applyOp(int a) {
        return Integer.lowestOneBit(a);
    }
}
