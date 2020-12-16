package expression;

public class Const implements CommonExpression {
    private double value;
    public Const() {
        this.value = 0;
    }
    public Const (int value) {
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
