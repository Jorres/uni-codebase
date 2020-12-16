package expression.exceptions;

public class Fragmentator {
    private int LEFT_OFFSET = 20, RIGHT_OFFSET = 5;
    public String getFragment(String s, int pos) {
        StringBuilder sb = new StringBuilder();
        sb.append("Pos = ");
        sb.append(pos);
        sb.append(": ");
        for (int i = pos - LEFT_OFFSET; i < pos + RIGHT_OFFSET; i++) {
            if (i >= 0 && i < s.length()) {
                sb.append(s.charAt(i));
            }
        }
        return sb.toString();
    }
}
