import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;

/**
 * As is, this code will get 84035 collisions with 216555 items.
 */
public class Main {
    public static void main(String[] args) {
        // This size is optimal for a load factor greater
        // than 0.5.
        Hashtable ht = new Hashtable(433099);

        try (BufferedReader br = new BufferedReader(new FileReader("/home/conor/dictionary.txt"))) {
            String word;
            System.out.print("Building hashtable ... ");
            double start = System.nanoTime();
            while ((word = br.readLine()) != null) {
                ht.insertDH(word);
            }
            double end = System.nanoTime();
            System.out.println("Done.");
            System.out.printf("Number of collisions: %d\n", ht.getCollisions());
        } catch (IOException e) {
            e.printStackTrace();
        }

        Scanner in = new Scanner(System.in);
        System.out.print("What word would you like to find? ");
        if (!ht.findDH(in.nextLine())) {
            System.out.println("Couldn't find word.");
        }

    }
}
