import java.util.*;

/*
    Sample input:

    4       // Number of links
    Data1   // Data
    Data2      ...
    Data3      ...
    Data4      ...
    3 0     // This sets link 3 to connect to link 0, thus creating
            // a loop of length 4. Set this to 0 -1 to create no loop.
*/

public class HR_Lab8 {
    public static void main(String[] args)
    {
        Scanner in = new Scanner(System.in);
        int num = Integer.parseInt(in.nextLine());
        Link[] array = new Link[num];

        for (int i = 0; i < num; i++) {
            array[i] = new Link(in.nextLine());
        }

        for (int i = 1; i < num; i++) {
            array[i - 1].next = array[i];
        }

        int select = in.nextInt();
        int next = in.nextInt();

        if (next != -1) {
            array[select].next = array[next];
        }

        LinkedList ll = new LinkedList();
        if (num > 0) {
            ll.first = array[0];
        }

        System.out.println(loopLength(ll));
    }

    static int loopLength(LinkedList ll) {
        if (ll.isEmpty())
            return 0;

        Link t = ll.first, h = ll.first;

        if (t.next == null)
            return 0;
        else {
            h = h.next.next;
        }

        int i = 0;
        while (true) {
            if (h == null || h.next == null)
                return 0;
            else if (t == h) {
                do {
                    t = t.next;
                    i++;
                } while (t != h);
                return i;
            }
            else {
                t = t.next;
                h = h.next.next;
            }
        }
    }
}

class Link {
    public String data;
    public Link next;

    public Link(String data) {
        this.data = data;
        next = null;
    }
}

class LinkedList {
    public Link first;

    public LinkedList() {
        first = null;
    }

    public boolean isEmpty() {
        return (first == null);
    }

    public void insertHead(Link insert) {
        if (this.isEmpty()) {
            first = insert;
        }
        else {
            insert.next = first;
            first = insert;
        }
    }
}

