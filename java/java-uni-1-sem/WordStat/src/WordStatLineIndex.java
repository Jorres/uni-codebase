import java.util.*;
import java.lang.*;
import java.io.*;

import javafx.util.Pair;

public class WordStatLineIndex {
    public static void main(String[] args) {
        try {
            ArrayList<Map.Entry<String, ArrayList<Pair<Integer, Integer>>>> ans = readWords(args[0]);
            printAnswer(args[1], ans);
        } catch (ArrayIndexOutOfBoundsException e) {
            exitWithMessage("One or more files are not specified. Usage: java WordStat <input> <output>");
        }
    }


    private static ArrayList<Map.Entry<String, ArrayList<Pair<Integer, Integer>>>> readWords(String fileName) {
        FastScanner sc = null;
        ArrayList<Map.Entry<String, ArrayList<Pair<Integer, Integer>>>> ans = new ArrayList<>();
        try {
            LinkedHashMap<String, ArrayList<Pair<Integer, Integer>>> map = new LinkedHashMap<>();
            sc = new FastScanner(new FileInputStream(new File(fileName)));
            int stringNum = 0;
            while (sc.hasNextChar()) {
                int curPos = 0;
                String cur = sc.nextLine();
                stringNum++;
                cur += '#';
                int prevStart = -1;
                for (int i = 0; i < cur.length(); i++) {
                    if (isWordSymbol(cur.charAt(i))) {
                        if (prevStart == -1) {
                            prevStart = i;
                        }
                    } else {
                        if (prevStart == -1) {
                            continue;
                        }

                        String curWord = cur.substring(prevStart, i).toLowerCase();
                        curPos++;

                        if (map.containsKey(curWord)) {
                            Pair<Integer, Integer> tmp = new Pair<>(stringNum, curPos);
                            map.get(curWord).add(tmp);
                            Pair<Integer, Integer> np = new Pair<>(map.get(curWord).get(0).getKey(),
                                                                   map.get(curWord).get(0).getValue() + 1);
                            map.get(curWord).set(0, np);
                        } else {
                            ArrayList<Pair<Integer, Integer>> newArray = new ArrayList<>();
                            Pair<Integer, Integer> tmp = new Pair<>(0, 1);
                            newArray.add(tmp);
                            Pair<Integer, Integer> firstWord = new Pair<>(stringNum, curPos);
                            newArray.add(firstWord);
                            map.put(curWord, newArray);
                        }

                        //putsIfAbsent
                        //getOrDefault

                        prevStart = -1;
                    }
                }
            }

            ans = new ArrayList<>(map.entrySet());
            ans.sort(Map.Entry.comparingByKey());
            sc.close();
        } catch (FileNotFoundException e) {
            exitWithMessage("\"" + fileName + "\" input file not found. Usage: java WordStat <input> <output>");
        }
        return ans;
    }

    private static boolean isWordSymbol(char c) {
        return (Character.isLetter(c) || c == '\'' || Character.getType(c) == Character.DASH_PUNCTUATION);
    }

    private static void printAnswer(String fileName, ArrayList<Map.Entry<String, ArrayList<Pair<Integer, Integer>>>> ans) {
        PrintWriter writer = null;
        try {
            writer = new PrintWriter(new File(fileName), "utf8");
            for (int i = 0; i < ans.size(); i++) {
                writer.print(ans.get(i).getKey() + " " + ans.get(i).getValue().get(0).getValue());

                for (int j = 1; j < ans.get(i).getValue().size(); j++) {
                    writer.print(" " + ans.get(i).getValue().get(j).getKey() + ":" + ans.get(i).getValue().get(j).getValue());
                }
                writer.println();
            }
            writer.close();
        } catch (FileNotFoundException e) {
            exitWithMessage("\"" + fileName + "\" input file not found. Usage: java WordStat <input> <output>");
        } catch (UnsupportedEncodingException e) {
            if (writer != null) {
                writer.close();
            }
            exitWithMessage("UTF-8 file encoding is not supported.");
        }
    }

    private static void exitWithMessage(String message) {
        System.out.println(message);
        System.exit(0);
    }
}
