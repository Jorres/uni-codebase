package md2html;

import java.util.Comparator;

public class Emphasis {
    public int start;
    public int len;
    public boolean isOpening;
    public Emphasis() {

    }
    public Emphasis(int _start, int _len) {
        start = _start;
        len = _len;
        isOpening = false;
    }

    public static Comparator<Emphasis> EPosComparator = new Comparator<Emphasis>() {

        public int compare(Emphasis s1, Emphasis s2) {
            int leftVal = s1.start;
            int rightVal = s2.start;

            return leftVal - rightVal;
        }};
}