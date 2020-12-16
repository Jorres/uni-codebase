public class Subtract extends Binary {
    public Subtract(Expression a, Expression b) {
        super(a, b);
    }

    public int applyOp(int x, int y) {
        return x - y;
    }


    public double applyOp(double x, double y) {
        return x - y;
    }
}
