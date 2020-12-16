package expression.operations;

import expression.CommonExpression;

public class unarMinus extends Unary {
    public unarMinus(CommonExpression e) {
        super(e);
    }

    public int applyOp(int a) {
        return -a;
    }
}
