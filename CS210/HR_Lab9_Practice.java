import java.util.Scanner;

public class HR_Lab9_Practice {
    public static void main(String[] args) {

        Scanner in = new Scanner(System.in);
        int n = Integer.parseInt(in.nextLine());
        System.out.printf("%.3f\n", 1 - birthday(n));
    }

    /* Calculates the probability that a birthday is NOT shared */
    static double birthday(int n) {
        if (n < 2) {
            return 1;
        }
        else {
            return (366 - (double)n) / 365 * birthday(n - 1);
        }
    }

    /* Calculates the probability directly */
    static double birthday2(int n) {
        
        double value;
        int x = n;
        
        if (x > 1) {
            return 1 - birthday(-n);
        }
        else if (x == -1) {
            return 1;
        }
        else {
            value = ((365 + (double)x + 1) / 365);
            x++;
        }
        return value*birthday(x);
    }
}
