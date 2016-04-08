/**
 * Class for creating hashtables. If you want to use different
 * hashing functions just change them in the insertDH and findDH
 * methods. I haven't implemented linear probing or quadratic
 * probing, just the double hashing collision resolution method.
 */
public class Hashtable {
    private int size;
    private String[] data;
    private int collisions;

    public Hashtable(int size) {
        this.size = size;
        this.data = new String[size];
        this.collisions = 0;
    }

    /**
     * Generates the table using a double hashing algorithm.
     * Best results are using oaat followed by fnv1a32.
     * @param s
     */
    public void insertDH(String s) {
        int key = Math.floorMod(oaat(s.toCharArray()), this.size);
        int step = Math.floorMod(fnv1a32(s.toCharArray()), this.size - 1) + 1;
        while (this.data[key] != null) {
            this.collisions++;
            key = Math.floorMod(key + step, this.size);
        }
        this.data[key] = s;
    }

    /**
     * Will fail if the table becomes full, but it won't.
     * @param s
     * @return true or false depending on whether the value was
     * found or not.
     */
    public boolean findDH(String s) {
        int key = Math.floorMod(oaat(s.toCharArray()), this.size);
        int step = Math.floorMod(fnv1a32(s.toCharArray()), this.size - 1) + 1;
        System.out.printf("Original key: %d\n", key);
        while (this.data[key] != null) {
            if (s.equals(this.data[key])) {
                System.out.printf("Found \"%s\" with key %d.\n", s, key);
                return true;
            }
            key = Math.floorMod(key + step, this.size);
        }
        return false;
    }

    public int getCollisions() {
        return this.collisions;
    }

    private static int fnv1a32(char[] cs) {
        long hash = 0x811c9dc5;
        for (char c : cs) {
            hash ^= c;
            hash = (hash * 0x01000193) & 0xffffffffL;
        }
        return (int) hash;
    }

    private static int djb2(char[] cs) {
        long hash = 5381;
        for (char c : cs) {
            hash = ((hash << 5) + hash) + c;
            hash &= 0xffffffffL;
        }
        return (int) hash;
    }

    private static int oaat(char[] cs) {
        long hash = 0;

        for (char c : cs) {
            hash += c;
            hash += (hash << 10);
            hash ^= (hash >> 6);
            hash &= 0xffffffffL;
        }
        hash += (hash << 3);
        hash ^= (hash >> 11);
        hash += (hash << 15);
        hash &= 0xffffffffL;
        return (int) hash;
    }

    private static int customHash(char[] cs) {
        long hash = 0;
        int pow = 1;
        for (char c : cs) {
            hash += (c - 'a') * pow;
            pow *= 26;
        }
        hash &= 0xffffffffL;

        return (int) hash;
    }
}
