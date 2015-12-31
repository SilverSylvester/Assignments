import java.util.Scanner;

public class Revision_06 {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = Integer.parseInt(in.nextLine());

        String[] cmds = new String[n];
        for (int i = 0; i < n; i++) {
            cmds[i] = in.nextLine();
        }

        Stack stack = new Stack(n);

        for (String cmd : cmds) {
            if (cmd.substring(0,3).equals("POP")) {
                if (!stack.isEmpty()) {
                    stack.pop();
                }
                else continue;
            }
            else if (cmd.substring(0,4).equals("PUSH")) {
                stack.push(Integer.parseInt(cmd.substring(5,cmd.length())));
            }
        }

        if (!stack.isEmpty()) {
            System.out.println(stack.peek());
        }
        else {
            System.out.println("empty");
        }
    }
}

class Stack {
    private int[] stack;
    private int size;
    private int head;

    public Stack(int size) {
        this.size = size;
        stack = new int[size];
        head = -1;
    }

    public void push(int n) {
        stack[++head] = n;
    }

    public int pop() {
        return stack[head--];
    }

    public int peek() {
        return stack[head];
    }

    public boolean isEmpty() {
        return (head == -1);
    }
}

