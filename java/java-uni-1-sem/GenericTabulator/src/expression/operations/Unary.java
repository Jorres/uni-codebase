package expression.operations;

import expression.CommonExpression;

public abstract class Unary implements CommonExpression {
    private CommonExpression e;

    public Unary(CommonExpression expr) {
        e = expr;
    }

    public int  evaluate(int  x) {
        return applyOp(e.evaluate(x));
    }
    public int  evaluate(int  x, int  y, int  z) {
        return applyOp(e.evaluate(x, y, z));
    }

    protected abstract int  applyOp(int  x);
}
