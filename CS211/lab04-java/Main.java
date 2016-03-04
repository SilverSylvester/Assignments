import java.util.*;
import java.io.*;

public class Main {
    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        String chars = in.nextLine();

        // Implementing comparator to sort by reverse string length (see
        // StringLengthComparator.java).
        Comparator<String> comp = new StringLengthComparator();

        // This is best implemented with a priority queue so we don't need
        // to sort anything later on.
        PriorityQueue<String> validWords = new PriorityQueue<>(comp);

        // HashMap for storing character counts for the string of characters
        // entered.
        HashMap<Character, Integer> charCounter = counts(chars);

        // Start the timer
        double start = System.nanoTime();

        try (BufferedReader br = new BufferedReader(new FileReader("/usr/share/dict/words"))) {
            String line; HashMap<Character, Integer> wordCounter;
            while ((line = br.readLine()) != null) {
                boolean canForm = true;
                wordCounter = counts(line);
                for (HashMap.Entry<Character, Integer> wc : wordCounter.entrySet()) {
                    if (charCounter.get(wc.getKey()) == null) {
                        canForm = false; break;
                    }
                    else if (charCounter.get(wc.getKey()) - wc.getValue() < 0) {
                        canForm = false; break;
                    }
                }
                if (canForm) validWords.add(line);

                // wordCounter is persistent over each iteration, so needs to be cleared.
                wordCounter.clear();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        // End the timer
        double end = System.nanoTime();

        System.out.println("Top ten suggestions:");
        for (int i = 0; i < 10 && !validWords.isEmpty(); i++) {
            System.out.println(validWords.poll());
        }
        // Average time seems to be around 0.14 to 0.16 seconds.
        System.out.printf("Time taken: %.3fs", (end - start) / 1000000000);
    }

    static HashMap<Character, Integer> counts(String chars) {
        char[] cs = chars.toCharArray();
        HashMap<Character, Integer> counter = new HashMap<>(chars.length());
        for (char c : cs) {
            if (counter.containsKey(c)) {
                counter.put(c, counter.get(c) + 1);
            }
            else {
                counter.put(c, 1);
            }
        }
        return counter;
    }
}
