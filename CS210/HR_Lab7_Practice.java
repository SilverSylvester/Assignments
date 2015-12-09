import java.util.Scanner;
import java.util.Arrays;

public class HR_Lab7_Practice {
    public static void main(String[] args) {
        
        Scanner in = new Scanner(System.in);
        int[] ns = new int[1];
        int n = Integer.parseInt(in.nextLine());
        ns[0] = n;
        
        while (in.hasNext()) {
            n = Integer.parseInt(in.nextLine());
            ns = insertQ(ns, n);
        }

        for (int i : ns) {
            System.out.println(i);
        }

    }

    static int[] insertQ(int[] ns, int n) {
        int[] xs = new int[ns.length + 1];
        for (int i = 0; i < ns.length; i++) {
            if (ns[i] <= n) {
                xs[i] = ns[i];
            }
            else {
                xs[i] = n;
                for (int j = i; j < ns.length; j++) {
                    xs[j + 1] = ns[j];
                }
                return xs;
            }
        }
        xs[xs.length - 1] = n;
        return xs;
    }
}

