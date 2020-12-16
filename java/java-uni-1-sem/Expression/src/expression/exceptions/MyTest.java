package expression.exceptions;

import expression.*;

import java.util.*;

public class MyTest {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        String s = sc.next();
        ExpressionParser parser = new ExpressionParser();

        CommonExpression t = null;

        try {
            t = parser.parse(s);
        } catch (MyParseException e) {
            System.out.println(e.getMessage());
        }

        System.out.println("x      f");
        for (int i = 0; i < 10; i++) {
            System.out.printf("%d   ", i);

            try {
                System.out.printf("%d\n", t.evaluate(i));
            } catch (IntegerOverflowException e) {
                System.out.println("overflow");
            } catch (NullPointerException e) {
                System.out.println("empty expression");
            } catch (DivisionByZeroException e) {
                System.out.println("division by zero");
            }
        }
    }
}
