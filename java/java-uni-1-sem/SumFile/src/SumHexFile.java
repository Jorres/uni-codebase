import java.util.*;
import java.io.*;
import java.lang.*;

public class SumHexFile {
    public static void main(String[] args) {
        try {
            int answer = readInts(args[0]);
            printAnswer(args[1], answer);
        } catch (ArrayIndexOutOfBoundsException e) {
            exitWithMessage("One or more files are not specified. Usage: java SumFile <input> <output>");
        }
    }

    private static int readInts(String fileName) {
        int answer = 0;
        Scanner sc = null;
        try {
            sc = new Scanner(new File(fileName), "utf8");
            while (sc.hasNext()) {
                String cur = sc.next().toLowerCase();
                if (cur.startsWith("0x")) {
                    cur = cur.substring(2);
                    answer += Integer.parseUnsignedInt(cur, 16);
                } else {
                    answer += Integer.parseInt(cur);
                }
            }
            sc.close();
        } catch (FileNotFoundException e) {
            exitWithMessage(fileName + " file not found. Usage: java SumFile <input> <output>");
        } catch (NumberFormatException e) {
            if (sc != null) {
                sc.close();
            }
            exitWithMessage("Number format error, integer in decimal or hexadecimal notation expected.");
        }
        return answer;
    }

    private static void printAnswer(String fileName, int value) {
        PrintWriter writer = null;
        try {
            writer = new PrintWriter(new OutputStreamWriter(new FileOutputStream(fileName)));
            writer.print(value);
            writer.close();
        } catch (FileNotFoundException e) {
            exitWithMessage("Output file not found. Usage: java SumFile <input> <output>");
        }
    }

    private static void exitWithMessage(String message) {
        System.out.println(message);
        System.exit(0);
    }
}