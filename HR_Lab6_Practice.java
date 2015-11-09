import java.util.Stack;
import java.util.Scanner;

public class HR_Lab6_Practice {
	public static void main(String[] args) {
		
		Scanner in = new Scanner(System.in);
		String s = in.nextLine().replace(" ", "");
		in.close();
		
		System.out.println(isPalindrome(s));
	}
	
	static boolean isPalindrome(String s) {
		Stack<Character> str = new Stack<>();
		String rev_s = new String();
		
		for (int i = 0; i < s.length(); i++) {
			str.push(s.charAt(i));
		}
		
		while (!str.isEmpty()) {
			rev_s += str.pop();
		}
		
		if (s.equals(rev_s))
			return true;
		else return false;
	}
}
