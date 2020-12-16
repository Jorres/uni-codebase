public class Const implements Expression, DoubleExpression, TripleExpression {
    private double value;
    Const() {
        this.value = 0;
    }
    Const (int value) {
        this.value = value;
    }
    public int evaluate(int x) {
        return (int)value;
    }
    public double evaluate(double x) {
        return value;
    }
    public int evaluate(int x, int y, int z) {
        return (int)value;
    }
}
