public class Variable implements Expression, DoubleExpression, TripleExpression {
    private String name;

    Variable() {
        this.name = "no_such_name";
    }

    Variable(String a) {
        this.name = a;
    }

    public int evaluate(int x) {
        return x;
    }

    public int evaluate(int x, int y, int z) {
        if (this.name.equals("x"))
            return x;
        if (this.name.equals("y"))
            return y;
        return z;
    }

    public double evaluate(double x) {
        return x;
    }
}
