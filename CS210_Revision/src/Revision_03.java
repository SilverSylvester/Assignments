import java.util.Scanner;
import java.util.HashMap;

public class Revision_03 {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = Integer.parseInt(in.nextLine());
        int[] ns = new int[n];
        for (int i = 0; i < n; i++) {
            ns[i] = in.nextInt();
        }

        System.out.println(mode(ns));
    }

    static int mode(int[] ns) {
        ns = reverse(ns); // A necessary evil, unfortunately.
        HashMap<Integer,Integer> hm = new HashMap<Integer,Integer>();
        int maxVal = 1, temp = 0;
        for (int i = 0; i < ns.length; i++) {
            if (hm.get(ns[i]) != null) {
                int count = hm.get(ns[i]);
                hm.put(ns[i], ++count);
                if (count >= maxVal) {
                    maxVal = count;
                    temp = ns[i];
                }
            }
            else {
                hm.put(ns[i], 1);
            }
        }
        return temp;
    }

    static int[] reverse(int[] ns) {
        int[] _ns = new int[ns.length]; int j = 0;
        for (int i = ns.length; i > 0; i--) {
            _ns[j++] = ns[i - 1];
        }
        return _ns;
    }
}
