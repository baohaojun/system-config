import java.util.HashMap;

public class Test {
    public static void main(String[] args) {
        HashMap<String, Integer> x = new HashMap<String, Integer>();
        x.put("hello", 1);
        x.put("world", 2);
        System.out.printf("%d\n", x.get("hello"));
    }
}
