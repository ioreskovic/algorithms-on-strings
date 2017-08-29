import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;

public class NonSharedSubstring implements Runnable {
    public static class Trie {
        Map<Character, Trie> children;
        Trie parent = null;
        Character parentChar = null;
        int mask = 0;

        Trie() {
            this.children = new HashMap<>(6);
        }

        public void consume(char[] letters, int start, int mask) {
            int i = start;
            Trie curr = this;
            while (i < letters.length) {
                curr.mask = curr.mask | mask;
                char c = letters[i];
                if (curr.children.containsKey(c)) {
                    curr = curr.children.get(c);
                } else {
                    Trie child = new Trie();
                    child.mask = mask;
                    child.parent = curr;
                    child.parentChar = c;
                    curr.children.put(c, child);
                    curr = child;
                }
                i++;
            }
        }
    }

    @Override
    public void run() {
        try {
            long start = 0L;
            long end = 0L;
            BufferedReader in = new BufferedReader (new InputStreamReader(System.in));
            start = System.currentTimeMillis();
            String text1 = in.readLine();
            end = System.currentTimeMillis();
//            System.out.println("Text1 Read Time: " + (end - start));

            start = System.currentTimeMillis();
            String text2 = in.readLine();
            end = System.currentTimeMillis();
//            System.out.println("Text2 Read Time: " + (end - start));

            Blah blah = compress(text1, text2);
            text1 = blah.ax;
            text2 = blah.bx;
            System.out.println("Blah");
            System.out.println(blah.toString());

            start = System.currentTimeMillis();
            String text = text1 + "#" + text2 + "$";
            end = System.currentTimeMillis();
//            System.out.println("Text Concat Time: " + (end - start));

            start = System.currentTimeMillis();
            String ans = solve(text.toCharArray());
            ans = reconstruct(ans, blah);
            end = System.currentTimeMillis();
//            System.out.println("Total Solve Time: " + (end - start));

            start = System.currentTimeMillis();
            System.out.println(ans);
            end = System.currentTimeMillis();
//            System.out.println("Print Result Time: " + (end - start));

        }
        catch (Throwable e) {
            e.printStackTrace ();
            System.exit (1);
        }
    }

    private String solve(char[] text) {
        long start = 0L;
        long end = 0L;
        Trie trie = new Trie();

        int len = text.length;
        int hLen = len / 2;

        start = System.currentTimeMillis();
        for (int i = 0; i < len; i++) {
            int mask = (i < hLen) ? 1 : 2;
            trie.consume(text, i, mask);
        }
        end = System.currentTimeMillis();
//        System.out.println("Trie Create Time: " + (end - start));

        start = System.currentTimeMillis();
        Deque<Trie> q = new ArrayDeque<>();
        q.offer(trie);
        Trie t = trie;
        while (!q.isEmpty()) {
            t = q.poll();
            if (t.mask == 1) {
                break;
            } else if (t.mask == 3) {
                for (Map.Entry<Character, Trie> entry : t.children.entrySet()) {
                    char key = entry.getKey();
                    if (key != '$' && key != '#') {
                        q.offer(entry.getValue());
                    }
                }
            }
        }
        end = System.currentTimeMillis();
//        System.out.println("Bfs Traverse Time: " + (end - start));

        start = System.currentTimeMillis();
        StringBuilder sb = new StringBuilder();
        while (t.parent != null) {
            sb.append(t.parentChar);
            t = t.parent;
        }
        end = System.currentTimeMillis();
//        System.out.println("Substring Reconstruction Time: " + (end - start));

        return sb.reverse().toString();
    }

    static class Blah {
        String ax;
        String bx;
        List<Character> mapC;
        List<Integer> mapI;

        public Blah(String ax, String bx, List<Character> mapC, List<Integer> mapI) {
            this.ax = ax;
            this.bx = bx;
            this.mapC = mapC;
            this.mapI = mapI;
        }

        @Override
        public String toString() {
            return "Blah{" +
                    "ax='" + ax + '\'' +
                    ", bx='" + bx + '\'' +
                    ", mapC=" + mapC +
                    ", mapI=" + mapI +
                    '}';
        }
    }

    public static Blah compress(String at, String bt) {
        List<Character> mapC = new ArrayList<>();
        List<Integer> mapI = new ArrayList<>();
        StringBuilder as = new StringBuilder();
        StringBuilder bs = new StringBuilder();

        int i = 1;
        char ap = at.charAt(0);
        char bp = bt.charAt(0);
        int len = 1;

        while (i < at.length() && i < bt.length()) {
            char ac = at.charAt(i);
            char bc = bt.charAt(i);

            if (ac == bc && ac == ap && bc == bp) {
                len = len + 1;
            } else {
                if (len > 1) {
                    as.append('!').append(mapC.size());
                    bs.append('!').append(mapC.size());
                    mapC.add(ap);
                    mapI.add(len);
                } else {
                    as.append(ap);
                    bs.append(bp);
                }
                len = 1;
            }

            ap = ac;
            bp = bc;
            i++;
        }
        if (len > 1) {
            mapC.add(ap);
            mapI.add(len);
            as.append('!').append(mapC.size());
            bs.append('!').append(mapC.size());
        } else {
            as.append(ap);
            bs.append(bp);
        }

        return new Blah(as.toString(), bs.toString(), mapC, mapI);
    }

    public static String reconstruct(String s, Blah blah) {
        StringBuilder sb = new StringBuilder();

        int i = 0;
        while (i < s.length()) {
            char c = s.charAt(i);
            if (c == '!') {
                int j = i + 1;
                char d = s.charAt(j);
                while (Character.isDigit(d)) {
                    j++;
                }
                String numS = s.substring(i + 1, j);
                int num = Integer.parseInt(numS);
                char repC = blah.mapC.get(num);
                int repI = blah.mapI.get(num);
                for (int k = 0; k < repI; k++) {
                    sb.append(repC);
                }
                i = j;
            } else {
                sb.append(c);
                i++;
            }
        }

        return sb.toString();
    }

    public static void main(String[] args) {
        new Thread(new NonSharedSubstring()).start();
    }
}
