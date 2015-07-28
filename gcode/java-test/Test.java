import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.Map;
import java.util.HashMap;

public class Test {
    public static void main(String[] args) {
        Map<String, Object> x = new HashMap<String, Object>();
        System.out.printf("hello %n");
        Pattern p = Pattern.compile("hello world (\\d+).*");
        Matcher m = p.matcher("hello world 2222\n");
        if (m.matches()) {
            System.out.printf("hello %n");
        }
    }
}
