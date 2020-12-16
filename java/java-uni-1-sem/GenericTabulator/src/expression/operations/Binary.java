package expression.operations;

import expression.CommonExpression;

public abstract class Binary implements CommonExpression {
    private CommonExpression left;
    private CommonExpression right;

    public Binary(CommonExpression a, CommonExpression b) {
        left = a;
        right = b;
    }

    public int evaluate(int x) {
        return applyOp(left.evaluate(x), right.evaluate(x));
    }

    public int evaluate(int x, int y, int z) {
        return applyOp(left.evaluate(x, y, z), right.evaluate(x, y, z));
    }

    protected abstract int applyOp(int x, int y);
}
