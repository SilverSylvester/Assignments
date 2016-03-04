import java.util.Random;

/**
 * Small class for generating random Scrabble hands
 * Created by conor on 04/03/16.
 */
public class ScrabbleHandGenerator {
    private static final char[] symbols;

    // List of characters, ['a'..'z']
    static {
        StringBuilder tmp = new StringBuilder();
        for (char c = 'a'; c <= 'z'; c++) {
            tmp.append(c);
        }
        symbols = tmp.toString().toCharArray();
    }

    // Instantiates PRNG and hand char array
    private final Random random = new Random();
    private final char[] hand;

    // Constructor
    public ScrabbleHandGenerator(int len) {
        if (len < 1) {
            throw new IllegalArgumentException("Length < 1: " + len);
        }
        hand = new char[len];
    }

    // Generates random hand
    public String genHand() {
        for (int i = 0; i < hand.length; i++) {
            hand[i] = symbols[random.nextInt(symbols.length)];
        }
        return new String(hand);
    }
}
