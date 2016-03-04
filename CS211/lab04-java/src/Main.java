import java.util.*;
import java.io.*;

/**
 * This program will prompt the user for a list of characters,
 * and (promptly) return a list of words that can be formed
 * by that character set, in order of their Scrabble scores.
 */
public class Main {
    public static void main(String[] args) {
        // Instantiating as much as possible before the REPL

        Scanner in = new Scanner(System.in);
        // Scrabble hand is 7 characters long
        ScrabbleHandGenerator generator = new ScrabbleHandGenerator(7);
        String chars;

        // Implementing comparator to sort by Scrabble score (see
        // ScrabbleScoreComparator.java).
        Comparator<String> comp = new ScrabbleScoreComparator();

        // This is best implemented with a priority queue so we don't need
        // to sort anything later on.
        PriorityQueue<String> validWords = new PriorityQueue<>(comp);

        REPL: while (true) {
            System.out.print("Random or custom (or quit)? [r|c] ");
            char ans = in.nextLine().charAt(0);

            switch (ans) {
                case 'r':
                    chars = generator.genHand();
                    break;
                case 'c':
                    chars = in.nextLine();
                    break;
                case 'q':
                    System.exit(0);
                default:
                    System.out.println("Invalid input.");
                    continue REPL;
            }

            System.out.printf("Your hand: %s\n", chars);

            // Creates a counter for storing character counts for the string of
            // characters entered.
            HashMap<Character, Integer> charCounter = counts(chars);

            // Start the timer
            double start = System.nanoTime();

            // Core idea: We generate character counts for both the input string and the word
            // in question, and essentially do a character count subtraction. If we have enough
            // characters after the subtraction completes, we can form that word.
            try (BufferedReader br = new BufferedReader(new FileReader("/usr/share/dict/words"))) {
                String line;
                HashMap<Character, Integer> wordCounter;
                // Iterating through every word in the dictionary
                while ((line = br.readLine()) != null) {
                    // Assume we can form the word, then try to prove otherwise
                    boolean canForm = true;
                    wordCounter = counts(line);
                    // We iterate through each entry in the wordCounter (the word in the dictionary)
                    for (HashMap.Entry<Character, Integer> wc : wordCounter.entrySet()) {
                        // If there exists a letter in the word that does not exist in our list of
                        // characters, then we can't possibly form the word from the characters, so
                        // we break out of the loop.
                        if (charCounter.get(wc.getKey()) == null) {
                            canForm = false;
                            break;
                        }
                        // If the character exists in both the list of characters and the word in the
                        // dictionary, we need to make sure we have enough of that particular character
                        // in the char list. If we don't (i.e. subtraction yields a negative value) then
                        // we can't form the word, and we break out of the loop.
                        else if (charCounter.get(wc.getKey()) - wc.getValue() < 0) {
                            canForm = false;
                            break;
                        }
                    }
                    // If the character list passes all the cases we covered above, then we must be able
                    // to form the word, otherwise this line is skipped.
                    if (canForm) validWords.add(line);

                    // The wordCounter HashMap is persistent over each iteration, so needs to be cleared
                    // for the next iteration.
                    wordCounter.clear();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }

            // End the timer
            double end = System.nanoTime();

            System.out.println("Top ten suggestions:");
            // Print out the first 10 valid words, or as much as
            // exists in the queue.
            for (int i = 0; i < 10 && !validWords.isEmpty(); i++) {
                System.out.println(validWords.poll());
            }
            System.out.printf("Time taken: %.3fs\n\n", (end - start) / 1000000000);

            // Need to clear this as well before the loop starts again.
            validWords.clear();
        }
    }

    /**
     * Creates a HashMap containing the characters in the String as keys, and
     * the number of times the character comes up as values.
     * @param chars
     * @return counter
     */
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
