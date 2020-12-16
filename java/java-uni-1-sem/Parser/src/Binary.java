public abstract class Binary implements Expression, DoubleExpression, TripleExpression {
    protected Expression left;
    protected Expression right;

    public Binary(Expression a, Expression b) {
        left = a;
        right = b;
    }

    public int evaluate(int x) {
        return applyOp(left.evaluate(x), right.evaluate(x));
    }
    public double evaluate(double x) {
        return applyOp(((DoubleExpression)left).evaluate(x),
                       ((DoubleExpression)right).evaluate(x));
    }
    public int evaluate(int x, int y, int z) {
        return applyOp(((TripleExpression)left).evaluate(x, y, z),
                       ((TripleExpression)right).evaluate(x, y, z));
    }

    protected abstract int applyOp(int x, int y);
    protected abstract double applyOp(double x, double y);
}
