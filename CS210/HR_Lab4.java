import java.util.Scanner;

public class HR_Lab4 {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = Integer.parseInt(in.nextLine());

        System.out.printf("%d\n", closest(n, sieve(2*n)));
    }

    static int[] sieve(int n) {
        boolean[] p = new boolean[n];
        p[0] = true; p[1] = true;
        int c = 2;
        for (int i = 2; i*i < n; i++) {
            if (!p[i]) {
                for (int j = 2*i; j < n; j += i) {
                    if (!p[j]) {
                        p[j] = true; c++;
                    }
                }
            }
        }

        int[] primes = new int[n - c];
        for (int i = 0, j = 0; i < n; i++) {
            if (!p[i]) {
                primes[j++] = i;
            }
        }
        return primes;
    }

    static int closest(int n, int[] ns) {
        int min = Integer.MAX_VALUE;
        int closest = n;
        for (int i : ns) {
            if (Math.abs(n - i) < min) {
                min = Math.abs(n - i);
                closest = i;
            }
            else break;
        }
        return closest;
    }
}
