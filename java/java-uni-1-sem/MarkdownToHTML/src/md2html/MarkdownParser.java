package md2html;

import java.util.*;
import java.io.*;

public class MarkdownParser {
    private HashMap<Character, String> specialCharacters;
    public MarkdownParser() {
        specialCharacters = new HashMap<>();
        specialCharacters.put('<', "&lt;");
        specialCharacters.put('>', "&gt;");
        specialCharacters.put('&', "&amp;");
    }

    public void parse(FastScanner sc, Writer wr) {
        ArrayList<String> curBlock = new ArrayList<>();
        while (getBlock(sc, curBlock) || curBlock.size() > 0) {
            try {
                processBlock(wr, curBlock);
            } catch (IOException e) {
                System.out.println("Error while writing to HTML file.");
            }
            curBlock.clear();
        }
        try {
            wr.flush();
        } catch (IOException e) {
            System.out.println("Could not flush BufferedWriter.");
        }
    }

    private boolean getBlock(FastScanner sc, ArrayList<String> block) {
        String s;
        boolean cont = true;
        while (true) {
            try {
                s = sc.nextLine();
            } catch (NoSuchElementException e) {
                cont = false;
                break;
            } catch (IOException e) {
                System.out.println("NextLine error");
                break;
            }
            if (isBlankString(s)) {
                break;
            }
            block.add(s);
        }
        return cont;
    }

    private boolean isBlankString(String s) {
        for (int i = 0; i < s.length(); i++) {
            if (!Character.isWhitespace(s.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private int ignorePicture(String data, int i) {
        if (data.charAt(i) == '!') {
            for (; i < data.length(); i++) {
                if (data.charAt(i) == ')') {
                    i++;
                    break;
                }
            }
        }
        return i;
    }

    private void processBlock(Writer wr, ArrayList<String> block) throws IOException {
        if (block.size() == 0) {
            return;
        }
        int hLevel = checkForHeader(block.get(0));
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < block.size(); i++) {
            sb.append(block.get(i));
            if (i != block.size() - 1) {
                sb.append(System.lineSeparator());
            }
        }
        String data = sb.toString();

        writeOpeningOrClosingTag(wr, hLevel, 0);

        ArrayList<Emphasis> a = new ArrayList<>();
        ArrayList<Emphasis> processing = new ArrayList<>();
        HashSet<Integer> deleted = new HashSet<>();

        int i = (hLevel != 0) ? hLevel + 1 : 0;

        for (; i < data.length(); i++) {
            i = ignorePicture(data, i);
            if (i == data.length()) {
                break;
            }

            int t = checkForEmphasis(data, i);
            if (t > 0) {
                Emphasis current = new Emphasis(i, t);
                Emphasis prev = (!processing.isEmpty()) ? processing.get(processing.size() - 1) : null;
                if (areEqualEmphasises(data, prev, current)) {
                    for (int k = 0; k < prev.len; k++) {
                        deleted.add(prev.start + k);
                    }
                    for (int k = 0; k < current.len; k++) {
                        deleted.add(current.start + k);
                    }
                    processing.remove(processing.size() - 1);

                    prev.isOpening = true;
                    a.add(prev);
                    current.isOpening = false;
                    a.add(current);
                } else {
                    processing.add(current);
                }

                i += t - 1;
            }
        }

        Collections.sort(a, Emphasis.EPosComparator);

        i = (hLevel != 0) ? hLevel + 1 : 0;
        int tagPos = 0;
        for (; i < data.length(); i++) {
            if (tagPos < a.size() && a.get(tagPos).start == i) {
                writeEmphasisTag(wr, a.get(tagPos), data);
                tagPos++;
            }
            if (!deleted.contains(i)) {
                if (parseImage(wr, data, i)) {
                    for (; i < data.length(); i++) {
                        if (data.charAt(i) == ')') {
                            break;
                        }
                    }
                    continue;
                }
                if (!checkForSpecial(wr, data.charAt(i))) {
                    if (data.charAt(i) == '\\') {
                        if (i == data.length() - 1) {
                            break;
                        }
                        char next = data.charAt(i + 1);
                        if (next == '*' || next == '_') {
                            wr.write(next);
                            i++;
                        }
                    } else {
                        wr.write(data.charAt(i));
                    }
                }
            }
        }

        for (; tagPos < a.size(); tagPos++) {
            writeEmphasisTag(wr, a.get(tagPos), data);
        }

        writeOpeningOrClosingTag(wr, hLevel, 1);
        wr.write(System.lineSeparator());
    }

    private boolean parseImage(Writer wr, String data, int pt) throws IOException {
        if (data.charAt(pt) != '!') {
            return false;
        }

        int i = pt + 1;
        for (; i < data.length(); i++) {
            if (data.charAt(i) == ']') {
                break;
            }
        }
        wr.write("<img alt='");
        wr.write(data.substring(pt + 2, i));
        wr.write("' src='");

        int start = i + 2;
        i += 2;
        for (; i < data.length(); i++) {
            if (data.charAt(i) == ')') {
                break;
            }
        }

        wr.write(data.substring(start, i));
        wr.write("'>");
        return true;
    }

    private void writeEmphasisTag(Writer wr, Emphasis e, String s) throws IOException {
        String type = "em";
        String closing = "";
        char c = s.charAt(e.start);
        if (e.len == 2) {
            type = "strong";
        }
        if (c == '-') {
            type = "s";
        }
        if (c == '`') {
            type = "code";
        }
        if (c == '~') {
            type = "mark";
        }
        if (!e.isOpening) {
            closing = "/";
        }
        wr.write("<" + closing + type + ">");
    }

    private boolean areEqualEmphasises(String s, Emphasis e1, Emphasis e2) {
        if (e1 == null ^ e2 == null) {
            return false;
        }
        if (e1.len != e2.len) {
            return false;
        }
        boolean eq = true;
        for (int i = 0; i < e1.len; i++) {
            eq &= s.charAt(e1.start + i) == s.charAt(e2.start + i);
        }
        return eq;
    }

    private int checkForHeader(String s) {
        int j = 0;
        for (; j < s.length() && s.charAt(j) == '#'; j++);

        if (j < s.length() && Character.isWhitespace(s.charAt(j))) {
            return j;
        }
        return 0;
    }

    private int checkForEmphasis(String s, int pt) {
        int ignored_as_emphasis = '0';
        int c1 = s.charAt(pt);
        int c2 = (pt + 1 < s.length()) ? s.charAt(pt + 1) : ignored_as_emphasis;
        if (c1 == '*') {
            return (c2 == c1) ? 2 : 1;
        }
        if (c1 == '_') {
            return (c2 == c1) ? 2 : 1;
        }
        if (c1 == '-') {
            return (c2 == '-') ? 2 : 0;
        }
        if (c1 == '`') {
            return 1;
        }
        if (c1 == '~') {
            return 1;
        }
        return 0;
    }

    private boolean checkForSpecial(Writer wr, char c) throws IOException {
        boolean t = specialCharacters.containsKey(c);
        if (specialCharacters.containsKey(c)) {
            wr.write(specialCharacters.get(c));
        }
        return t;
    }

    private void writeOpeningOrClosingTag(Writer wr, int t, int which) throws IOException {
        String s = "";
        if (which == 1) {
            s = "/";
        }
        if (t > 0) {
            wr.write("<" + s + "h" + t + ">");
        } else {
            wr.write("<" + s + "p>");
        }
    }
}