import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TrieMatching implements Runnable {
    public static class Trie {
        private String word;
        private Map<Character, Trie> children;
        Trie parent = null;


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

        public void consume(char[] letters) {
            int i = 0;
            Trie curr = this;
            while (i < letters.length) {
                char c = letters[i];
                if (curr.children.containsKey(c)) {
                    curr = curr.children.get(c);
                } else {
                    Trie child = new Trie(curr.word + String.valueOf(c), new HashMap<>(4));
                    child.parent = this;
                    curr.setChild(c, child);
                    curr = child;
                }
                i++;
            }
        }
    }

    public List<Integer> trieMatching(char[] text, Trie trie) {
        int i = 0;
        List<Integer> results = new ArrayList<>();
        while (i < text.length) {
            if (prefixTrieMatching(i, text, trie)) {
                results.add(i);
            }
            i++;
        }

        return results;
    }

    public boolean prefixTrieMatching(int start, char[] text, Trie trie) {
//        System.out.println("Strating prefix matching for: " + String.valueOf(text, start, text.length - start));
        int i = start;
        Trie node = trie;

        while (i < text.length) {
            char c = text[i];
            if (node.children.isEmpty()){
                return true;
            } else if (node.children.containsKey(c)) {
                node = node.children.get(c);
                i++;
            } else {
                return false;
            }
        }

        return node.children.isEmpty();
    }

    public void run () {
        try {
            BufferedReader in = new BufferedReader (new InputStreamReader(System.in));
            String text = in.readLine ();
            int n = Integer.parseInt (in.readLine ());
            List <String> patterns = new ArrayList <String> ();
            for (int i = 0; i < n; i++) {
                patterns.add (in.readLine ());
            }

            List <Integer> ans = solve(text, n, patterns);

            for (int j = 0; j < ans.size (); j++) {
                System.out.print ("" + ans.get (j));
                System.out.print (j + 1 < ans.size () ? " " : "\n");
            }
        }
        catch (Throwable e) {
            e.printStackTrace ();
            System.exit (1);
        }
    }

    private List<Integer> solve(String text, int n, List<String> patterns) {
        Trie trie = new Trie("", new HashMap<>(4));
        int i = 0;
        while (i < n) {
            trie.consume(patterns.get(i).toCharArray());
            i++;
        }

        return trieMatching(text.toCharArray(), trie);
    }

    public static void main (String [] args) {
        new Thread (new TrieMatching ()).start ();
    }
}
