package expression.parser;

import expression.*;
import expression.exceptions.*;
import expression.operations.*;

import java.util.*;

public class ExpressionParser implements Parser {
    private HashMap<String, Integer> prior = new HashMap<>();
    private Fragmentator fr = new Fragmentator();
    private void initMap() {
        prior.put("+", 3);
        prior.put("-", 3);
        prior.put("*", 2);
        prior.put("/", 2);
        prior.put("umin", 0);
        prior.put("high", 0);
        prior.put("low", 0);
        prior.put("abs", 0);
        prior.put("sqrt", 0);
        prior.put("log2", 0);
        prior.put("pow2", 0);
        prior.put("(", 11);
        prior.put(")", 10);
        prior.put("&", 4);
        prior.put("^", 5);
        prior.put("|", 6);
        prior.put("~", 0);
        prior.put("count", 0);
    }

    private void convert(Stack<CommonExpression> vars, Stack<Token> opers, String s) {
        if (vars.size() < 2) {
            throw new OperandNotFoundException("Not enough operands for: " + fr.getFragment(s, opers.peek().endPos));
        }
        Token op = opers.pop();
        CommonExpression vRight = vars.pop();
        CommonExpression vLeft = vars.pop();

        switch (op.t) {
            case "+":
                vars.push(new CheckedAdd(vLeft, vRight));
                break;
            case "-":
                vars.push(new CheckedSubtract(vLeft, vRight));
                break;
            case "*":
                vars.push(new CheckedMultiply(vLeft, vRight));
                break;
            case "/":
                vars.push(new CheckedDivide(vLeft, vRight));
                break;
            case "|":
                vars.push(new BinOr(vLeft, vRight));
                break;
            case "&":
                vars.push(new BinAnd(vLeft, vRight));
                break;
            case "^":
                vars.push(new BinXor(vLeft, vRight));
                break;
            default:
                throw new UnknownTokenException("Unknown token " + op + ": " + fr.getFragment(s, op.endPos));
        }
    }

    private void applyUnary(Stack<CommonExpression> vars, Stack<Token> opers) {
        if (vars.size() < 1) {
            throw new OperandNotFoundException("Invalid expression: lack of operands.");
        }
        Token op = opers.pop();

        CommonExpression e = vars.pop();

        switch (op.t) {
            case "low":
                vars.push(new Low(e));
                break;
            case "high":
                vars.push(new High(e));
                break;
            case "sqrt":
                vars.push(new Sqrt(e));
                break;
            case "abs":
                vars.push(new Abs(e));
                break;
            case "pow2":
                vars.push(new Pow2(e));
                break;
            case "log2":
                vars.push(new Log2(e));
                break;
            case "umin":
                vars.push(new CheckedNegate(e));
                break;
            case "~":
                vars.push(new unarNot(e));
                break;
            case "count":
                vars.push(new Count(e));
                break;
            default:
                vars.push(e);
                opers.push(op);
                break;
        }
    }

    public CommonExpression parse(String s) {
        initMap();
        Stack<CommonExpression> vars = new Stack<>();
        Stack<Token> opers = new Stack<>();

        int pt = -1;

        TokenString ts = new TokenString();
        ts.opBefore = true;
        ts.initSet();

        while (pt <= s.length()) {
            MyPair response = ts.nextToken(s, pt);
            Token cur = response.getKey();
            pt = response.getValue();

            if (cur.name != ID.Other) {
                if (cur.name == ID.Num) {
                    try {
                        vars.push(new Const(Integer.parseInt(cur.t)));
                    } catch (NumberFormatException e) {
                        throw new ConstantParsingException("Constant overflow: " + fr.getFragment(s, pt));
                    }
                } else {
                    vars.push(new Variable(cur.t));
                }
                ts.opBefore = false;
            } else {
                if (prior.get(cur.t) == 0) {
                    opers.push(cur);
                } else {
                    if (cur.t.equals("-") && ts.opBefore) {
                        cur.t = "umin";
                        opers.push(cur);
                    } else {
                        if (!cur.t.equals("(")) {
                            while (!opers.empty() && prior.get(opers.peek().t) <= prior.get(cur.t)) {
                                int t = opers.size();
                                applyUnary(vars, opers);
                                if (opers.size() == t) {
                                    convert(vars, opers, s);
                                }
                            }
                        }

                        if (cur.t.equals(")")) {
                            ts.opBefore = false;
                            if (opers.size() == 0) {
                                throw new BracketsPlacementException("Invalid brackets placement: " + fr.getFragment(s, pt));
                            }
                            opers.pop();
                        } else {
                            opers.push(cur);
                        }
                    }
                    if (!(cur.t.equals("(") || cur.t.equals(")"))) {
                        ts.opBefore = true;
                    }
                }
            }
        }

        if (opers.size() != 0) {
            throw new OperandNotFoundException("Lack of operands for: " + fr.getFragment(s, opers.peek().endPos));
        }

        if (vars.size() != 1) {
            throw new OperationNotFoundException("Lack of expression.operations");
        }

        return vars.peek();
    }
}