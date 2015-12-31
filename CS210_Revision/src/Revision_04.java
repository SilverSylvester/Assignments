import java.util.Scanner;

public class Revision_04 {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = Integer.parseInt(in.nextLine());

        if (isPrime(n)) System.out.println("TRUE");
        else System.out.println("FALSE");
    }

    static boolean isPrime(int n) {
        if (n % 2 == 0) return false;
        for (int i = 3; i*i <= n; i += 2) {
            if (n % i == 0) return false;
        }
        return true;
    }
}
