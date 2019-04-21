package streams;

import java.util.Scanner;

/**
 * @author zenind
 */
public class Launcher {

    public static void main(String[] args) {
        int i = 4;
        double d = 4.0;
        String s = "HackerRank ";

        Scanner scan = new Scanner(System.in);
        /* Declare second integer, double, and String variables. */
        int i2 = 0;
        double d2 = 0.0;
        String s2 = null;

        /* Read and save an integer, double, and String to your variables.*/
        if (scan.hasNextInt()) {
            i2 = scan.nextInt();
        }
        if (scan.hasNextDouble()) {
            d2 = scan.nextDouble();
        }
        if (scan.hasNext()) {
            s2 = scan.next();
        }
        /* Print the sum of both integer variables on a new line. */
        System.out.println(i+i2);

        /* Print the sum of the double variables on a new line. */
        System.out.println(d+d2);

        /* Concatenate and print the String variables on a new line;
        	the 's' variable above should be printed first. */
        System.out.println(s+s2);
    }
}
