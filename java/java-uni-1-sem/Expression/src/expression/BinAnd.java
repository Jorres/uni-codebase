package expression;

public class BinAnd extends Binary {
    public BinAnd(CommonExpression a, CommonExpression b) {
        super(a, b);
    }

    public int applyOp(int x, int y) {
        return x & y;
    }
}
