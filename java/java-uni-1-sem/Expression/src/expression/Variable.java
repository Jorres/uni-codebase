package expression;

public class Variable implements CommonExpression {
    private String name;

    public Variable() {
        this.name = "no_such_name";
    }

    public Variable(String a) {
        this.name = a;
    }

    public int  evaluate(int  x) {
        return x;
    }

    public int  evaluate(int  x, int  y, int  z) {
        int  tmp[] = {x, y, z};
        return tmp[this.name.charAt(0) - 'x'];
    }

    public double evaluate(double x) {
        return x;
    }
}
