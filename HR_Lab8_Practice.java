import java.util.*;

/*
    Sample Input:

    4
    Data1
    Data2
    Data3
    Data4
    0 -1  1  // Current link | Previous Link | Next link
    1  0  2
    2  1  3
    3  2 -1

    Will output TRUE, since the list is properly connected.
*/

public class HR_Lab8_Practice {
    public static void main(String[] args)
    {
        Scanner in = new Scanner(System.in);
        int num = Integer.parseInt(in.nextLine());
        Link[] array = new Link[num];

        for (int i = 0; i < num; i++) {
            array[i] = new Link(in.nextLine());    
        }

        while (in.hasNext()) {
            int select = in.nextInt();
            int previous = in.nextInt();
            int next = in.nextInt();
            
            if (previous != -1) {
                array[select].previous = array[previous];
            }

            if (next != -1){
                array[select].next = array[next];
            }
        }

        LinkedList ll = new LinkedList();
        
        if (num > 0) {
            ll.first = array[0];
            ll.last = array[num - 1];
        }

        System.out.println(check(ll));
    }

    public static String check(LinkedList ll) {
        
        if (ll.isEmpty()) return "empty";

        Link f = ll.first;

        if (f.previous != null) return "FALSE";

        while (true) {
            if (f.next == null) return "TRUE";
            else if (f != f.next.previous) {
                return "FALSE";
            }
            f = f.next;
        }
    }
}

class Link {
	public String data;
	public Link next;
	public Link previous;
    
    public Link(String input){
        data = input;
        next = null;
        previous = null;
    }
}

class LinkedList {
	public Link first;
    public Link last;

	public LinkedList() {
		first = null;
        last = null;
	}

	public boolean isEmpty(){
		return (first == null);
	}
    
    public void insertHead(Link insert){
        if(isEmpty()) {
            first = insert;
            last = insert;
        }
        else {
            first.previous = insert;
            insert.next = first;
            first = insert;
        }
    }
}

