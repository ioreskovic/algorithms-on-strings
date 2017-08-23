import java.util.*;

class SuffixTree {
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
        SuffixTree tree = this;

        if (!cbx.isEmpty()) {
            if (tree.isEmpty()) {
                tree.links.put(cbx, new SuffixTree(position));
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
            } else {
                ax.append(cax.substring(i));
                bx.append(cbx.substring(i));
                break;
            }
        }

        return new CxAxBx(cx.toString(), ax.toString(), bx.toString());
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
}
