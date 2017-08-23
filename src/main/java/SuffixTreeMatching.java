import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;

public class SuffixTreeMatching implements Runnable {

    static class SuffixTree {
        Map<String, SuffixTree> links = new HashMap<>();
        Integer position = null;

        public SuffixTree() {
        }

        public SuffixTree(int position) {
            this.position = position;
        }

        SuffixTree withLink(String s, SuffixTree subTree) {
            links.put(s, subTree);
            return this;
        }

        boolean isEmpty() {
            return links.isEmpty();
        }

        SuffixTree consume(String cbx, int position) {
            if (!cbx.isEmpty()) {
                if (isEmpty()) {
                    links.put(cbx, new SuffixTree(position));
                    return this;
                } else {
                    String cax = findWithHead(links.keySet(), cbx.charAt(0));
                    if (cax == null) {
                        links.put(cbx, new SuffixTree(position));
                        return this;
                    } else {
                        CxAxBx r = branch(cax, cbx);
                        if (r.ax.isEmpty() && r.bx.isEmpty()) {
                            return this;
                        } else if (r.ax.isEmpty()) {
                            links.put(r.cx, links.get(r.cx).consume(r.bx, position));
                            return this;
                        } else if (r.bx.isEmpty()) {
                            throw new IllegalArgumentException("Should not happen");
                        } else {
                            SuffixTree old = links.remove(cax);
                            links.put(r.cx, new SuffixTree().withLink(r.ax, old).consume(r.bx, position));
                            return this;
                        }
                    }
                }
            }

            return this;
        }

        List<String> edges() {
            Deque<SuffixTree> q = new ArrayDeque<>();
            q.offer(this);
            List<String> edges = new ArrayList<>();

            while (!q.isEmpty()) {
                SuffixTree head = q.poll();
                if (!head.isEmpty()) {
                    for (Map.Entry<String, SuffixTree> entry : head.links.entrySet()) {
                        edges.add(entry.getKey());
                        q.offer(entry.getValue());
                    }
                }
            }

            return edges;
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder("Node");
            if (isEmpty()) {
                sb.append("(").append(position).append(")");
            } else {
                sb.append("[");
                int i = 0;
                for (Map.Entry<String, SuffixTree> entry : links.entrySet()) {
                    sb.append(entry.getKey()).append(" -> ").append(entry.getValue().toString());
                    if (i < links.size() - 1) {
                        sb.append(", ");
                    }
                    i++;
                }
                sb.append("]");
            }

            return sb.toString();
        }

        static String findWithHead(Iterable<String> strings, char head) {
            for (String s : strings) {
                if (s.charAt(0) == head) {
                    return s;
                }
            }
            return null;
        }

        static CxAxBx branch(String cax, String cbx) {
            int i = 0;
            StringBuilder cx = new StringBuilder();
            StringBuilder ax = new StringBuilder();
            StringBuilder bx = new StringBuilder();

            while (i < cax.length() && i < cbx.length()) {
                char a = cax.charAt(i);
                char b = cbx.charAt(i);
                if (a == b) {
                    cx.append(a);
                    i++;
                } else {
                    break;
                }
            }

            ax.append(cax.substring(i));
            bx.append(cbx.substring(i));

            return new CxAxBx(cx.toString(), ax.toString(), bx.toString());
        }

        static SuffixTree create(String s) {
            SuffixTree tree = new SuffixTree();
            for (int i = 0; i < s.length(); i++) {
                tree.consume(s.substring(i), i);
            }
            return tree;
        }
    }

    static class CxAxBx {
        CxAxBx(String cx, String ax, String bx) {
            this.cx = cx;
            this.ax = ax;
            this.bx = bx;
        }

        String cx;
        String ax;
        String bx;
    }

    @Override
    public void run () {
        try {
            BufferedReader in = new BufferedReader (new InputStreamReader(System.in));
            String text = in.readLine ();

            List <String> ans = solve(text);

            for (int j = 0; j < ans.size (); j++) {
                System.out.println(ans.get(j));
            }
        } catch (Throwable e) {
            e.printStackTrace ();
            System.exit (1);
        }
    }

    private List<String> solve(String text) {
        SuffixTree tree = SuffixTree.create(text);
        return tree.edges();
    }

    public static void main(String[] args) {
        new Thread(new SuffixTreeMatching()).start();
    }
}
