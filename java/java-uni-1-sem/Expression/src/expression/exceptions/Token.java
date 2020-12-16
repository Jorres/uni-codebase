package expression.exceptions;

public class Token {
    public ID name;
    public String t;

    public int endPos;

    public Token() {
        t = "";
        name = ID.Other;
    }
}
