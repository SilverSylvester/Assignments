import java.util.ArrayDeque;
import java.util.Scanner;

public class HR_Lab6 {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        ArrayDeque<Integer> stack = new ArrayDeque<Integer>();
        int n = Integer.parseInt(in.nextLine());
        String[][] cmds = new String[n][2];

        for (int i = 0; i < n; i++) {
            cmds[i] = in.nextLine().split(" ");
        }

        ArrayDeque<Integer> list = parseIn(cmds, stack);
        if (list.isEmpty()) {
            System.out.println("empty");
        }
        else System.out.printf("%d\n", max(list));
    }

    static ArrayDeque<Integer> parseIn(String[][] cmds, ArrayDeque<Integer> stack) {
        for (String[] cmd : cmds) {
            if (cmd[0].equals("PUSH")) {
                stack.addFirst(Integer.parseInt(cmd[1]));
            }
            else if (stack.isEmpty()) {
                continue;
            }
            else {
                stack.removeFirst();
            }
        }
        return stack;
    }

    static int max(ArrayDeque<Integer> stack) {
        int max = stack.removeFirst();
        while (!stack.isEmpty()) {
            if (stack.peekFirst() > max) {
                max = stack.removeFirst();
            }
            else {
                stack.removeFirst();
            }
        }
        return max;
    }
}
