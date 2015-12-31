import java.util.Scanner;

public class Revision_05 {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = Integer.parseInt(in.nextLine());
        
        System.out.println(closest(n, sieve(n)));
    }
    
    static int[] sieve(int n) {
        boolean[] isComp = new boolean[n];
        isComp[0] = true; isComp[1] = true; isComp[2] = false;
        for (int i = 2; i*i <= n; i++) {
            if (!isComp[i]) {
                for (int j = 2*i; j*i <= n; j += i) {
                    isComp[j] = true;
                }
            }
        }

        int ps = 0;
        for (boolean p : isComp) {
            if (!p) ps++;
        }

        int[] primes = new int[ps]; int j = 0;
        for (int i = 0; i < n; i++) {
            if (!isComp[i]) primes[j++] = i;
        }
        return primes;
    }
    
    static int closest(int n, int[] ns) {
        int diff = Math.abs(n - ns[0]), index = 0;
        for (int i = 1; i < ns.length; i++) {
            if (diff < Math.abs(n - ns[i])) {
                diff = Math.abs(n - ns[i]);
                index = i;
            }
        }
        return ns[index];
    }
}

