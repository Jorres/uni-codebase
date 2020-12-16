package md2html;

import md2html.FastScanner;
import md2html.MarkdownParser;
import org.junit.Assert;
import org.junit.Test;

import java.io.*;

import java.nio.charset.StandardCharsets;
import java.util.NoSuchElementException;

public class MarkdownParserTest {
    @Test
    public void testing() {
        String s = System.lineSeparator();
        String sep =  System.lineSeparator();
        /*testParse("I love plain text", "<p>I love plain text</p>" + s);
        testParse("#I love plain text", "<p>#I love plain text</p>" + s);
        testParse("# I love multiple " + sep + " layered headers", "<h1>I love multiple " + s + " layered headers</h1>" + s);
        testParse("### I love multiple leveled headers", "<h3>I love multiple leveled headers</h3>" + s);
        testParse("# Let's see how" + sep + sep + "### You will deal with" + sep + sep + "##### A lot of headers",
                "<h1>Let's see how</h1>" + s + "<h3>You will deal with</h3>" + s + "<h5>A lot of headers</h5>" + s);
        testParse("*lott at me, I'm an emphasis*", "<p><em>lott at me, I'm an emphasis</em></p>" + s);
        testParse("**strong one**", "<p><strong>strong one</strong></p>" + s);
        testParse("*testing __inner__*", "<p><em>testing <strong>inner</strong></em></p>" + s);
        testParse("--crossing--", "<p><s>crossing</s></p>" + s);
        testParse("-not crossing-", "<p>-not crossing-</p>" + s);
        testParse("### we are just single poor * and _", "<h3>we are just single poor * and _</h3>" + s);
        testParse("and that's how we write `code`", "<p>and that's how we write <code>code</code></p>" + s);
        testParse("check < " + sep + "and > " + sep + "and &.", "<p>check &lt; " + s + "and &gt; " + s + "and &amp;.</p>" + s);
        testParse("![a](b)****", "<p><img alt='a' src='b'></p>" + s);*/

        testParse("![a __b__](b)", "<p><img alt='a __b__' src='b'></p>" + s);

        /*testParse("reverse slash \\*", "<p>reverse slash *</p>" + s);

        testParse(
                sep + sep + sep + sep + "Лишние пустые строки должны игнорироваться." + sep + sep + sep + sep,
                "<p>Лишние пустые строки должны игнорироваться.</p>" + s
        );

        testParse("![my image](my reference)", "<p><img alt='my image' src='my reference'></p>" + s);*/
    }

    private void testParse(String src, String ans) {
        MarkdownParser parser = new MarkdownParser();

        BufferedWriter wr = null;
        try {
            wr =  new BufferedWriter(new OutputStreamWriter(new FileOutputStream("html_file.html"), StandardCharsets.UTF_8));
        } catch (IOException e) {
            System.out.println("Error while opening BufferedWriter into HTML file.");
            return;
        }
        FastScanner into = new FastScanner(src);
        parser.parse(into, wr);

        FastScanner out = null;
        try {
            out = new FastScanner(new FileInputStream(new File("html_file.html")));
        } catch (IOException e) {
            System.out.println("Error while opening md2html.FastScanner from HTML file.");
            return;
        }

        StringBuilder sb = new StringBuilder();
        while (true) {
            try {
                String tmp = out.nextLine();
                sb.append(tmp);
                sb.append(System.lineSeparator());
            } catch (NoSuchElementException e) {
                break;
            } catch (IOException e) {
                System.out.println("NextLine error");
            }
        }

        Assert.assertEquals(ans, sb.toString());
    }
}