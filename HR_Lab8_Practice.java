import java.util.*;

public class HR_Lab8_Practice {
    public static void main(String[] args)
    {
        Scanner in = new Scanner(System.in);
        int num = Integer.parseInt(in.nextLine());
        Link[] array = new Link[num];

        for(int i = 0; i < num; i++) {
            array[i]=new Link(in.nextLine());    
        }

        while(in.hasNext()) {
            int select = in.nextInt();
            int previous = in.nextInt();
            int next = in.nextInt();
            
            if(previous != -1) {
                array[select].previous = array[previous];
            }

            if(next != -1){
                array[select].next = array[next];
            }
        }

        LinkedList ll = new LinkedList();
        
        if(num > 0) {
            ll.first = array[0];
            ll.last = array[num - 1];
        }

        System.out.println(check(ll));
    }

    public static String check(LinkedList ll) {
        
        if (ll.isEmpty()) return "empty";

        Link f = ll.first;

        /* In reality, this check is only probabilistic, failing for lists
         * that are incorrectly linked but have the same elements throughout.
         * 
         * It is also dependent on the size of the list.
         */
        for (int i = 0; i < 101; i++) {
            if (f.next == null) break;
            else if (!f.data.equals(f.next.previous.data)) {
                return "FALSE";
            }
            f = f.next;
        }
        return "TRUE";
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

