import java.util.*;

public class TrieMatching {
    static abstract class Trie {
        boolean isEmpty() { return links().isEmpty(); }
        abstract Map<Character, Trie> links();
        boolean hasLink(char c) { return links().containsKey(c); }
        Trie consume(char[] chars, int i, int x) {
            Trie t = this;

            while (i < chars.length - 1) {
                char c = chars[i];
                Trie newNode;
                if (t.hasLink(c)) {
                    t = t.links().get(c);
                } else {
                    newNode = new Leaf(x);
                    t.links().put(c, newNode);
                    t = newNode;
                }
                i = i + 1;
            }

            t.links().put(chars[i], new Leaf(x));

            return this;
        }
        Trie consume(String s, int x) { return consume(s.toCharArray(), 0, x); }
        List<Integer> locations(String s) { return locations((s + '$').toCharArray()); }
        List<Integer> locations(char[] chars) {
            int i = 0;
            Trie t = this;
            List<Integer> res = new ArrayList<>();
            while (i < chars.length) {
                char c = chars[i];
                if (i == chars.length - 1 && c == '$') {
                    Deque<Trie> tx = new ArrayDeque<>();
                    tx.offer(t);

                    while (!tx.isEmpty()) {
                        Trie y = tx.poll();
                        if (y.isEmpty()) {
                            res.add(((Leaf) y).i);
                        } else {
                            for (Trie x : y.links().values()) {
                                tx.offer(x);
                            }
                        }
                    }

                    return res;
                } else if (t.hasLink(c)) {
                    t = t.links().get(c);
                    i = i + 1;
                } else {
                    return res;
                }
            }

            return res;
        }
    }

    static class Node extends Trie {
        final Map<Character, Trie> links;

        Node() {
            this(new HashMap<>());
        }

        Node(Map<Character, Trie> links) {
            this.links = links;
        }

        @Override
        Map<Character, Trie> links() {
            return links;
        }

//        @Override
//        Trie consume(char[] chars, int i, int x) {
//            if (i < chars.length) {
//                char c = chars[i];
//                Trie newNode;
//                if (hasLink(c)) {
//                    newNode = links.get(c).consume(chars, i + 1, x);
//                } else {
//                    newNode = new Leaf(x).consume(chars, i + 1, x);
//                }
//                links.put(c, newNode);
//                return this;
//            } else {
//                return this;
//            }
//        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder();

            sb.append("Node[");

            for (Map.Entry<Character, Trie> entry : links.entrySet()) {
                sb.append(entry.getKey()).append(" -> ").append(entry.getValue().toString()).append(",");
            }

            sb.deleteCharAt(sb.length() - 1);
            sb.append("]");

            return sb.toString();
        }
    }

    static class Leaf extends Trie {
        final int i;
        final Map<Character, Trie> links;

        Leaf(int i) {
            this.i = i;
            this.links = new HashMap<>();
        }

        @Override
        Map<Character, Trie> links() {
            return links;
        }

//        @Override
//        Trie consume(char[] chars, int i, int x) {
//            if (i < chars.length) {
//                char c = chars[i];
//                Trie leaf = new Leaf(x).consume(chars, i + 1, x);
//                Map<Character, Trie> newLinks = new HashMap<>();
//                newLinks.put(c, leaf);
//                return new Node(newLinks);
//            } else {
//                return this;
//            }
//        }

        @Override
        public String toString() {
            return "Leaf[" + i + "]";
        }
    }

    static Trie suffix(String text) {
        Trie trie = new Node();
        for (int i = 0; i < text.length(); i++) {
            String s = text.substring(i) + '$';
            trie = trie.consume(s, i);
        }
        trie = trie.consume("$", text.length());
        return trie;
    }

    public static void main(String[] args) {
        Scanner stdin = new Scanner(System.in);
        String text = stdin.nextLine();
        int nPatterns = Integer.parseInt(stdin.nextLine());
        List<String> patterns = new ArrayList<>();
        for (int i = 0; i < nPatterns; i++) {
            patterns.add(stdin.nextLine());
        }
        Trie trie = suffix(text);

//        System.out.println(trie.toString());

        Set<Integer> locationSet = new TreeSet<>();

        for (String pattern : patterns) {
            List<Integer> patLocs = trie.locations(pattern);
//            System.out.println("Locations for pattern " + pattern + ": " + patLocs.toString());
            locationSet.addAll(patLocs);
        }

        for (Integer loc : locationSet) {
            System.out.print(loc + " ");
        }
        System.out.println();
    }
}
