/*import java.util.*;
import java.io.*;
import java.lang.*;

public class SumAbcFile {
    public static void main(String[] args) {
        try {
            int sum = readInts(args[0]);
            printAnswer(args[1], sum);
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
                String cur = sc.next();
                answer += Integer.parseInt(cur);
            }
            sc.close();
        } catch (FileNotFoundException e) {
            exitWithMessage(fileName + " file not found. Usage: java SumFile <input> <output>");
        } catch (NumberFormatException e) {
            if (sc != null) {
                sc.close();
            }
            exitWithMessage("Number format error!");
        }
        return answer;
    }

    private static void printAnswer(String fileName, int value) {
        BufferedWriter writer = null;
        try {
            writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(fileName)));
            writer.write(Integer.toString(value));
        } catch (FileNotFoundException e) {
            exitWithMessage("Output file not found. Usage: java SumFile <input> <output>");
        } catch(IOException e) {
            exitWithMessage("Unknown error, could not create writer.");
        }

        try {
            if (writer != null) {
                writer.close();
            }
        } catch (IOException ex) {
            exitWithMessage("Unknown error, could not close writer.");
        }
    }

    private static void exitWithMessage(String message) {
        System.out.println(message);
        System.exit(0);
    }
}*/