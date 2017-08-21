import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.*;

public class SuffixArrayJava implements Runnable {

    public static void main (String [] args) {
        new Thread (new SuffixArrayJava ()).start ();
    }

    public static Map<Character, Integer> x = new HashMap<>();

    static {
        x.put('$', 0);
        x.put('A', 1);
        x.put('C', 2);
        x.put('G', 3);
        x.put('T', 4);
    }

    @Override
    public void run() {
        try {
            BufferedReader in = new BufferedReader (new InputStreamReader(System.in));
            String s = in.readLine();
//            System.out.println(suffixArrayString(suffixArray(buildSuffixMap(s))));
//            System.out.println(suffixString(buildSuffixMap(s)));
            System.out.println(suffixStringPrimitive(buildSuffixArray(s)));
        }
        catch (Throwable e) {
            e.printStackTrace ();
            System.exit (1);
        }
    }

    public Map<Integer, Integer> sortCharacters(String s) {
        Map<Integer, Integer> order = new HashMap<>();
        Map<Character, Integer> count = new HashMap<>();

        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            count.put(c, count.getOrDefault(c, 0) + 1);
        }

        List<Character> alphabet = new ArrayList<>(count.keySet());
        Collections.sort(alphabet);

        for (int j = 1; j < alphabet.size(); j++) {
            char currSymbol = alphabet.get(j);
            char prevSymbol = alphabet.get(j - 1);
            count.put(currSymbol, count.get(currSymbol) + count.get(prevSymbol));
        }

        for (int i = s.length() - 1; i >= 0; i--) {
            char c = s.charAt(i);
            count.put(c, count.get(c) - 1);
            order.put(count.get(c), i);
        }

        return order;
    }

    public int[] sortCharactersPrimitive(String s) {
        int[] order = new int[s.length()];
        int[] count = new int[x.size()];

        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            count[x.get(c)] = count[x.get(c)] + 1;
        }

        for (int j = 1; j < x.size(); j++) {
            count[j] = count[j] + count[j - 1];
        }

        for (int i = s.length() - 1; i >= 0; i--) {
            char c = s.charAt(i);
            count[x.get(c)] = count[x.get(c)] - 1;
            order[count[x.get(c)]] = i;
        }

        return order;
    }

    public Map<Integer, Integer> computeCharClasses(String s, Map<Integer, Integer> order) {
        Map<Integer, Integer> classes = new HashMap<>();
        classes.put(order.get(0), 0);

        for (int i = 1; i < s.length(); i++) {
            if (s.charAt(order.get(i)) != s.charAt(order.get(i - 1))) {
                classes.put(order.get(i), classes.get(order.get(i - 1)) + 1);
            } else {
                classes.put(order.get(i), classes.get(order.get(i - 1)));
            }
        }

        return classes;
    }

    public int[] computeCharClassesPrimitive(String s, int[] order) {
        int[] classes = new int[s.length()];
        classes[order[0]] = 0;

        for (int i = 1; i < s.length(); i++) {
            if (s.charAt(order[i]) != s.charAt(order[i - 1])) {
                classes[order[i]] = classes[order[i - 1]] + 1;
            } else {
                classes[order[i]] = classes[order[i - 1]];
            }
        }

        return classes;
    }

    public Map<Integer, Integer> sortDoubled(String s, Integer l, Map<Integer, Integer> order, Map<Integer, Integer> classes) {
        Map<Integer, Integer> count = new HashMap<>();
        Map<Integer, Integer> newOrder = new HashMap<>();

        for (int i = 0; i < s.length(); i++) {
            count.put(classes.get(i), count.getOrDefault(classes.get(i), 0) + 1);
        }

        for (int j = 1; j < s.length(); j++) {
            count.put(j, count.getOrDefault(j, 0) + count.get(j - 1));
        }

        for (int i = s.length() - 1; i >= 0; i--) {
            int start = (order.get(i) - l + s.length()) % s.length();
            int cl = classes.get(start);
            count.put(cl, count.get(cl) - 1);
            newOrder.put(count.get(cl), start);
        }

        return newOrder;
    }

    public int[] sortDoubledPrimitive(String s, Integer l, int[] order, int[] classes) {
        int[] count = new int[s.length()];
        int[] newOrder = new int[s.length()];

        for (int i = 0; i < s.length(); i++) {
            count[classes[i]] = count[classes[i]] + 1;
        }

        for (int j = 1; j < s.length(); j++) {
            count[j] = count[j] + count[j - 1];
        }

        for (int i = s.length() - 1; i >= 0; i--) {
            int start = (order[i] - l + s.length()) % s.length();
            int cl = classes[start];
            count[cl] = count[cl] - 1;
            newOrder[count[cl]] = start;
        }

        return newOrder;
    }

    public Map<Integer, Integer> updateClasses(Map<Integer, Integer> newOrder, Map<Integer, Integer> classes, Integer l) {
        int n = newOrder.size();
        Map<Integer, Integer> newClasses = new HashMap<>();
        newClasses.put(newOrder.get(0), 0);

        for (int i = 1; i < n; i++) {
            int curr = newOrder.get(i);
            int prev = newOrder.get(i - 1);
            int midCurr = (curr + l) % n;
            int midPrev = (prev + l) % n;

            if ((classes.get(curr) != classes.get(prev)) || (classes.get(midCurr) != classes.get(midPrev))) {
                newClasses.put(curr, newClasses.get(prev) + 1);
            } else {
                newClasses.put(curr, newClasses.get(prev));
            }
        }

        return newClasses;
    }

    public int[] updateClassesPrimitive(int[] newOrder, int[] classes, Integer l) {
        int n = newOrder.length;
        int[] newClasses = new int[n];
        newClasses[newOrder[0]] = 0;

        for (int i = 1; i < n; i++) {
            int curr = newOrder[i];
            int prev = newOrder[i - 1];
            int midCurr = (curr + l) % n;
            int midPrev = (prev + l) % n;

            if ((classes[curr] != classes[prev]) || (classes[midCurr] != classes[midPrev])) {
                newClasses[curr] = newClasses[prev] + 1;
            } else {
                newClasses[curr] = newClasses[prev];
            }
        }

        return newClasses;
    }

    public Map<Integer, Integer> buildSuffixMap(String s) {
        Map<Integer, Integer> order = sortCharacters(s);
        Map<Integer, Integer> classes = computeCharClasses(s, order);

        int l = 1;

        while (l < s.length()) {
            order = sortDoubled(s, l, order, classes);
            classes = updateClasses(order, classes, l);
            l = l * 2;
        }

        return order;
    }

    public int[] buildSuffixArray(String s) {
        int[] order = sortCharactersPrimitive(s);
        int[] classes = computeCharClassesPrimitive(s, order);

        int l = 1;

        while (l < s.length()) {
            order = sortDoubledPrimitive(s, l, order, classes);
            classes = updateClassesPrimitive(order, classes, l);
            l = l * 2;
        }

        return order;
    }

    public List<Integer> suffixArray(Map<Integer, Integer> suffixMap) {
        List<Integer> suffixArray = new ArrayList<>();

//        TreeSet<Integer> keys = new TreeSet<>(suffixMap.keySet());

        for (int i = 0; i < suffixMap.size(); i++) {
            suffixArray.add(suffixMap.get(i));
        }

//        for (Integer k : keys) {
//            suffixArray.add(suffixMap.get(k));
//        }

        return suffixArray;
    }

    public String suffixArrayString(List<Integer> suffixArray) {
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < suffixArray.size(); i++) {
            sb.append(suffixArray.get(i));
            if (i < suffixArray.size() - 1) {
                sb.append(" ");
            }
        }

        return sb.toString();
    }

    public String suffixString(Map<Integer, Integer> suffixMap) {
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < suffixMap.size(); i++) {
            sb.append(suffixMap.get(i));
            if (i < suffixMap.size() - 1) {
                sb.append(' ');
            }
        }

        return sb.toString();
    }

    public String suffixStringPrimitive(int[] suffixArray) {
        StringBuilder sb = new StringBuilder();

        for (int i = 0; i < suffixArray.length; i++) {
            sb.append(suffixArray[i]);
            if (i < suffixArray.length - 1) {
                sb.append(' ');
            }
        }

        return sb.toString();
    }
}
