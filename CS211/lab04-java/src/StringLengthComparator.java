import java.util.Comparator;

/**
 * Custom comparator for the priority queue, ensures the Strings
 * are polled in reverse order on comparing their length.
 */
public class StringLengthComparator implements Comparator<String> {
    @Override
    public int compare(String a, String b) {
        return (b.length() - a.length());
    }
}
