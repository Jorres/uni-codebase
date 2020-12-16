package expression;

public class unarNot extends Unary {
    public unarNot(CommonExpression e) {
        super(e);
    }

    public int applyOp(int a) {
        return ~a;
    }
}
