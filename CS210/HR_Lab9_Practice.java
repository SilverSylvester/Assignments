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
        
        if (n > 0) {
            return 1 - birthday2(-n);
        }
        else if (n == -1) {
            return 1;
        }
        else {
            return (365 + (double)n + 1) / 365 * birthday2(n + 1);
        }  
    }
}
