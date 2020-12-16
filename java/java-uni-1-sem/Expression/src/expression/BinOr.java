package expression;

public class BinOr extends Binary {
    public BinOr(CommonExpression a, CommonExpression b) {
        super(a, b);
    }

    public int applyOp(int x, int y) {
        return x | y;
    }
}

