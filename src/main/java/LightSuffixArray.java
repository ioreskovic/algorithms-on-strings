import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;

public class LightSuffixArray implements Runnable {
    private static class Node {
        static final int Letters = 6;
        static final int NA = -1;
        int next[];
        int start;
        int offset;
        int generalStart;
        int id;
        boolean haveNeighbours;

        Node() {
            next = new int[Letters];
            Arrays.fill(next, NA);
        }


        Node(int start, int offset, int id) {
            this();
            this.start = start;
            this.offset = offset;
            this.id = id;
        }

        public void initNext() {
            Arrays.fill(next, NA);
            haveNeighbours = false;
        }

        List<Integer> getNeighbours() {
            List<Integer> result = new ArrayList<>();
            for (int aNext : next) {
                if (aNext > 0) result.add(aNext);
            }
            return result;
        }
    }

    int letterToIndex(char letter) {
        switch (letter) {
            case 'A':
                return 0;
            case 'C':
                return 1;
            case 'G':
                return 2;
            case 'T':
                return 3;
            case '$':
                return 4;
            default:
                assert (false);
                return Node.NA;
        }
    }

    List<Node> textToTree(String text) {
        List<Node> tree = new ArrayList<>();
        int count = 0;
        tree.add(new Node(0, -1, count++));
        int length = text.length();

        for (int j = 0; j < length; j++) {
            int initialStart = length - 1 - j;
            int initialOffset = j;
            Node currentNode = tree.get(0);
            while (currentNode.next[letterToIndex(text.charAt(initialStart))] > 0) {
                currentNode = tree.get(currentNode.next[letterToIndex(text.charAt(initialStart))]);
                int currentStart = currentNode.start;
                int currentOffset = currentNode.offset;
                int removeIndex = 1;
                for (int i = 1; i < currentOffset + 1; i++) {
                    if (text.charAt(currentStart + i) != text.charAt(initialStart + i)) {
                        break;
                    }
                    removeIndex++;
                }

                if (currentOffset + 1 - removeIndex > 0) {
                    Node newNode = new Node(currentStart + removeIndex, currentOffset - removeIndex, count++);
                    newNode.generalStart = currentNode.generalStart;
                    currentNode.start = initialStart;
                    currentNode.offset = removeIndex - 1;
                    tree.add(newNode);
                    if (currentNode.haveNeighbours) {
                        newNode.next = Arrays.copyOf(currentNode.next, currentNode.next.length);
                        newNode.haveNeighbours = true;
                        currentNode.initNext();
                    }
                    currentNode.next[letterToIndex(text.charAt(newNode.start))] = newNode.id;
                    currentNode.haveNeighbours = true;
                }
                initialStart += removeIndex;
                initialOffset -= removeIndex;
            }
            Node newNode = new Node(initialStart, initialOffset, count++);
            newNode.generalStart = length - 1 - j;
            tree.add(newNode);
            currentNode.next[letterToIndex(text.charAt(initialStart))] = newNode.id;
            currentNode.haveNeighbours = true;
        }
        return tree;
    }

    List<Integer> suffixArray(String text, List<Node> tree) {
        Deque<Integer> deque = new ArrayDeque<>();
        TreeMap<String, Integer> suffixes = new TreeMap<>();
        deque.offer(0);

        while (!deque.isEmpty()) {
            int pos = deque.poll();
            Node currentNode = tree.get(pos);
            if (currentNode.haveNeighbours) {
                for (int nPos : currentNode.getNeighbours()) {
                    deque.offer(nPos);
                }
            } else {
                suffixes.put(text.substring(currentNode.generalStart, currentNode.start + currentNode.offset + 1), currentNode.generalStart);
            }
        }

        return new ArrayList<>(suffixes.values());
    }

    @Override
    public void run() {
        try {
            BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
            String text = in.readLine();
            List<Integer> result = suffixArray(text, textToTree(text));

            for (int i = 0; i < result.size(); i++) {
                System.out.print(result.get(i));
                if (i < result.size() - 1) {
                    System.out.println(" ");
                }
            }

            System.out.println();
        } catch (Throwable e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    public static void main(String[] args) {
        new Thread(new LightSuffixArray()).start();
    }
}
