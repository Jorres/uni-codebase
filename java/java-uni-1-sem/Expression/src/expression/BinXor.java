package expression;

public class BinXor extends Binary {
    public BinXor(CommonExpression a, CommonExpression b) {
        super(a, b);
    }

    public int applyOp(int x, int y) {
        return x ^ y;
    }
}

