import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;

public class NonSharedSubstring implements Runnable {
    public static class Trie {
        private String word;
        private Map<Character, Trie> children;
        Trie parent = null;
        Character parentChar = null;
        int mask = 0;


        public Trie(String word, Map<Character, Trie> children) {
            this.word = word;
            this.children = children;
        }

        public String getWord() {
            return word;
        }

        public Map<Character, Trie> getChildren() {
            return children;
        }

        public void setWord(String word) {
            this.word = word;
        }

        public void setChildren(Map<Character, Trie> children) {
            this.children = children;
        }

        public void setChild(Character c, Trie child) {
            this.children.put(c, child);
        }

        public void consume(char[] letters, int mask) {
            int i = 0;
            Trie curr = this;
            while (i < letters.length) {
                curr.mask = curr.mask | mask;
                char c = letters[i];
                if (curr.children.containsKey(c)) {
                    curr = curr.children.get(c);
                } else {
                    Trie child = new Trie(curr.word + String.valueOf(c), new HashMap<>(6));
                    child.mask = mask;
                    child.parent = this;
                    child.parentChar = c;
                    curr.setChild(c, child);
                    curr = child;
                }
                i++;
            }
        }
    }

    @Override
    public void run() {
        try {
            BufferedReader in = new BufferedReader (new InputStreamReader(System.in));
            String text1 = in.readLine();
            String text2 = in.readLine();
            String text = text1 + "#" + text2 + "$";

            String ans = solve(text);
            System.out.println(ans);
        }
        catch (Throwable e) {
            e.printStackTrace ();
            System.exit (1);
        }
    }

    private String solve(String text) {
        Trie trie = new Trie("", new HashMap<>(6));
        for (int i = 0; i < text.length(); i++) {
            int mask = (i < text.length() / 2) ? 1 : 2;
            trie.consume(text.substring(i).toCharArray(), mask);
        }

        Deque<Trie> q = new ArrayDeque<>();
        q.offer(trie);

        Trie t = trie;

        while (!q.isEmpty()) {
            t = q.poll();
            if (t.mask == 1) {
                break;
            }

            for (Map.Entry<Character, Trie> entry : t.children.entrySet()) {
                char key = entry.getKey();
                if (key != '$' && key != '#') {
                    q.offer(entry.getValue());
                }
            }
        }

        return t.word;
    }

    public static void main(String[] args) {
        new Thread(new NonSharedSubstring()).start();
    }
}
