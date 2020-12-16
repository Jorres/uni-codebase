package md2html;

import java.io.*;
import java.nio.charset.StandardCharsets;

public class Md2Html {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Usage: java Md2Html <md_file.md> <html_file.html>");
            return;
        }

        try (FastScanner sc = new FastScanner(new FileInputStream(args[0]))) {
            BufferedWriter wr;
            try {
                wr =  new BufferedWriter(new OutputStreamWriter(new FileOutputStream(args[1]), StandardCharsets.UTF_8));
            } catch (FileNotFoundException e) {
                System.out.println("HTML file was not found. Usage: java Md2Html <md_file.md> <html_file.html>");
                return;
            }
            MarkdownParser parser = new MarkdownParser();
            try {
                parser.parse(sc, wr);
                wr.close();
            } catch (IOException e) {
                System.out.println("Unable to close writer.");
            }
        } catch (FileNotFoundException e) {
            System.out.println("Markdown file was not found. Usage: java Md2Html <md_file.md> <html_file.html>");
        }
    }
}