package expression.exceptions;

import expression.CommonExpression;
import expression.Unary;

public class Low extends Unary {
    public Low(CommonExpression e) {
        super(e);
    }
    public int applyOp(int a) {
        return Integer.lowestOneBit(a);
    }
}
