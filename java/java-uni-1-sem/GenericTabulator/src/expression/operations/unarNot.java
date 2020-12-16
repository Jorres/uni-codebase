package expression.operations;

import expression.CommonExpression;

public class unarNot extends Unary {
    public unarNot(CommonExpression e) {
        super(e);
    }

    public int applyOp(int a) {
        return ~a;
    }
}
