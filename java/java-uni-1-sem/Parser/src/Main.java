public class Main {
    public static void main(String[] args) {
        try {
            int x = Integer.parseInt(args[0]);
            System.out.println(x * x - 2 * x + 1);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Incorrect input.\n");
        }
    }
}
