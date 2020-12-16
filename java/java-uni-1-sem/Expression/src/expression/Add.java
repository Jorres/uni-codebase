package expression;

public class Add extends Binary {
    public Add(CommonExpression a, CommonExpression b) {
        super(a, b);
    }

    public int applyOp(int x, int y) {
        return x + y;
    }
    public double applyOp(double x, double y) {
        return x + y;
    }
}