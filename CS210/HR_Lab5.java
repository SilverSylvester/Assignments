import java.util.*;

public class HR_Lab5 {
	public static void main(String[] args) {
		
		// ---------------- I/O ------------------ //

		Scanner in = new Scanner(System.in);
		int n = Integer.parseInt(in.nextLine()); // We don't really need this

		String[] ss = in.nextLine().split(" ");
		int[] ns = new int[n];
		for (int i = 0; i < n; i++) {
			ns[i] = Integer.parseInt(ss[i]);
		}

		// --------------- LOGIC ---------------- //

		System.out.printf("List: %s\nMedian: %.1f\n", Arrays.toString(ns), median(ns));
	}

	static double median(int[] ns) {
		int len = ns.length;
		ns = sort(ns);
		if (len % 2 == 0) {
			return (double)(ns[len / 2 - 1] + ns[len / 2]) / 2;
		}
		else {
			return ns[len / 2];
		}
	}

	static int[] sort(int[] ns) {
		boolean swapping = false;
		int temp;
		do {
			swapping = false;
			for (int i = 1; i < ns.length; i++) {
				if (ns[i - 1] > ns[i]) {
					temp = ns[i - 1];
					ns[i - 1] = ns[i];
					ns[i] = temp;
					swapping = true;
				}
			}
		} while (swapping);
		return ns;
	}
}

