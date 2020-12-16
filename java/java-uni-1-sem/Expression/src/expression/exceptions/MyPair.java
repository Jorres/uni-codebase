package expression.exceptions;

public class MyPair {
    Token key;
    int value;

    public MyPair(Token _key, int _value) {
        key = _key;
        value = _value;
    }

    public Token getKey() {
        return key;
    }

    public int getValue() {
        return value;
    }
}
