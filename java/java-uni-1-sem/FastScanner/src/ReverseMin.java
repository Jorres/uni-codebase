import java.util.*;
import java.lang.*;

public class ReverseMin {
    public static void main(String[] args) {
        FastScanner sc = new FastScanner(System.in);
        ArrayList<ArrayList<Integer>> data = new ArrayList<>();
        int maxRowLen = Integer.MIN_VALUE;
        try {
            while (true) {
                data.add(new ArrayList<>());
                String temp = sc.nextLine() + " ";

                int begin = 0;
                boolean isNum = false;
                for (int i = 0; i < temp.length(); i++) {
                    char c = temp.charAt(i);
                    if ((Character.isDigit(c) || c == '-') && !isNum) {
                        isNum = true;
                        begin = i;
                    } else if (!(Character.isDigit(c) || c == '-') && isNum) {
                        data.get(data.size() - 1).add(Integer.parseInt(temp.substring(begin, i)));
                        isNum = false;
                    }
                }
                maxRowLen = Math.max(data.get(data.size() - 1).size(), maxRowLen);
            }
        } catch (NoSuchElementException e) {
            sc.close();
        }

        data.remove(data.size() - 1);

        ArrayList<Integer> minRow = new ArrayList<>();
        ArrayList<Integer> minCol = new ArrayList<>();

        for (int i = 0; i < data.size(); i++) {
            minRow.add(Integer.MAX_VALUE);
        }

        for (int i = 0; i < maxRowLen; i++) {
            minCol.add(Integer.MAX_VALUE);
        }

        for (int i = 0; i < data.size(); i++) {
            for (int j = 0; j < data.get(i).size(); j++) {
                int val = data.get(i).get(j);
                if (minRow.get(i) > val) {
                    minRow.set(i, val);
                }
                if (minCol.get(j) > val) {
                    minCol.set(j, val);
                }
            }
        }

        for (int i = 0; i < data.size(); i++) {
            for (int j = 0; j < data.get(i).size(); j++) {
                System.out.print(Math.min(minRow.get(i), minCol.get(j)) + " ");
            }
            System.out.println();
        }
    }
}
