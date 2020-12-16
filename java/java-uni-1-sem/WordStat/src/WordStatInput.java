/*import java.util.*;
import java.lang.*;
import java.io.*;

public class WordStatInput {
    public static void main(String[] args) {
        try {
            ArrayList<Map.Entry<String, Integer>> ans = readWords(args[0]);
            printAnswer(args[1], ans);
        } catch (ArrayIndexOutOfBoundsException e) {
            exitWithMessage("One or more files are not specified. Usage: java WordStat <input> <output>");
        } catch (Exception any) {
            exitWithMessage("What a pity, you've lost because of unknown reason.");
        }
    }

    private static ArrayList<Map.Entry<String, Integer>> readWords(String fileName) {
        Scanner sc = null;
        ArrayList<Map.Entry<String, Integer>> ans = new ArrayList<>();
        try {
            LinkedHashMap<String, Integer> map = new LinkedHashMap<>();
            sc = new Scanner(new File(fileName), "utf-8");
            while (sc.hasNextLine()) {
                String cur = sc.nextLine();
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

                        if (map.containsKey(curWord)) {
                            map.put(curWord, 1 + map.get(curWord));
                        } else {
                            map.put(curWord, 1);
                        }
                        prevStart = -1;
                    }
                }
            }

            ans = new ArrayList<>(map.entrySet());
            //ans.sort(Map.Entry.comparingByKey());
            sc.close();
        } catch (FileNotFoundException e) {
            exitWithMessage("\"" + fileName + "\n input file not found. Usage: java WordStat <input> <output>");
        }
        return ans;
    }

    private static boolean isWordSymbol(char c) {
        return (Character.isLetter(c) || c == '\'' || Character.getType(c) == Character.DASH_PUNCTUATION);
    }

    private static void printAnswer(String fileName, ArrayList<Map.Entry<String, Integer>> ans) {
        PrintWriter writer = null;
        try {
            writer = new PrintWriter(new File(fileName), "utf8");
            for (int i = 0; i < ans.size(); i++) {
                writer.print(ans.get(i).getKey() + " " + ans.get(i).getValue() + '\n');
            }
            writer.close();
        } catch (FileNotFoundException e) {
            exitWithMessage("\"" + fileName + "\n input file not found. Usage: java WordStat <input> <output>");
        } catch (UnsupportedEncodingException e) {
            exitWithMessage("UTF-8 file encoding is not supported.");
        } finally {
            if (writer != null) {
                writer.close();
            }
        }
    }

    private static void exitWithMessage(String message) {
        System.out.println(message);
        System.exit(0);
    }
}*/
