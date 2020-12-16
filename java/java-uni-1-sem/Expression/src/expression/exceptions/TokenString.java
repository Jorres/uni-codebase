package expression.exceptions;

import java.util.*;

public class TokenString {
    public boolean opBefore;
    private HashSet<String> operations = new HashSet<>();
    private Fragmentator fr = new Fragmentator();
    public void initSet() {
        operations.add("+");
        operations.add("-");
        operations.add("*");
        operations.add("/");
        operations.add("&");
        operations.add("|");
        operations.add("~");
        operations.add("^");
        operations.add("x");
        operations.add("y");
        operations.add("z");
        operations.add("(");
        operations.add("log2");
        operations.add("abs");
        operations.add("sqrt");
        operations.add("pow2");
        operations.add("high");
        operations.add("low");
        operations.add(")");
        operations.add("count");
    }
    private final int maxOpLen = 5;

    private final int LEFT_OFFSET = 20, RIGHT_OFFSET = 5;

    private int skipSpaces(String s, int pt) {
        while (pt < s.length() && Character.isWhitespace(s.charAt(pt))) {
            pt++;
        }
        return pt;
    }

    public MyPair nextToken(String s, int pt) {
        Token ans = new Token();
        StringBuilder sb = new StringBuilder();
        if (pt == -1) {
            pt++;
            ans.t = "(";
            pt = skipSpaces(s, pt);
            ans.endPos = pt;
            return new MyPair(ans, pt);
        }
        if (pt == s.length()) {
            pt++;
            ans.t = ")";
            ans.endPos = pt;
            return new MyPair(ans, pt);
        }

        if (Character.isDigit(s.charAt(pt))) {
            while (pt < s.length() && Character.isDigit(s.charAt(pt))) {
                sb.append(s.charAt(pt++));
            }
            ans.t = sb.toString();
            ans.name = ID.Num;
            ans.endPos = pt;
            pt = skipSpaces(s, pt);
        } else if (Character.isLetter(s.charAt(pt))) {
            int i;
            for (i = pt; i - pt < maxOpLen && i < s.length(); i++) {
                char c = s.charAt(i);
                if (!Character.isDigit(c) && !Character.isLetter(c)) {
                    break;
                }
                sb.append(c);
            }
            if (operations.contains(sb.toString())) {
                ans.t = sb.toString();
                pt = i;
                ans.endPos = pt;
                pt = skipSpaces(s, pt);
                if (ans.t.length() == 1 && ans.t.charAt(0) >= 'x' && ans.t.charAt(0) <= 'z') {
                    ans.name = ID.Var;
                }
            } else {
                throw new UnknownTokenException("Invalid expression: " + fr.getFragment(s, pt));
            }
        } else {
            int i;
            for (i = pt; i - pt < maxOpLen && i < s.length(); i++) {
                sb.append(s.charAt(i));
                if (operations.contains(sb.toString())) {
                    ans.t = sb.toString();
                    pt = i + 1;
                    ans.endPos = pt;
                    pt = skipSpaces(s, pt);
                    break;
                }
            }

            if (ans.t.equals("-") && opBefore) {
                StringBuilder sec = new StringBuilder();
                sec.append("-");
                if (pt < s.length() && Character.isDigit(s.charAt(pt))) {
                    while (pt < s.length() && Character.isDigit(s.charAt(pt))) {
                        sec.append(s.charAt(pt++));
                    }
                    ans.t = sec.toString();
                    ans.name = ID.Num;
                    ans.endPos = pt;
                    pt = skipSpaces(s, pt);
                }
            }

            if (ans.t.equals("")) {
                throw new UnknownTokenException("Unknown symbols in expression: " + fr.getFragment(s, pt));
            }
        }

        return new MyPair(ans, pt);
    }
}
