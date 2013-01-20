import java.util.*;
class Test implements TestInterface {
    interface TestInterface {
	void interfaceFunc();
    }

    void interfaceFunc() {
	System.out.println("interfaceFunc");
    }
    public static void main(String []args) {
	new Test2().interfaceFunc();
	System.out.println("hello of Test");
	char[] s = "hello of Test".toCharArray();
	for (char c : s) {
	    System.out.printf("hello %c\n", c);
	}
    }
    void hello(Test x, Object[] args) {
	System.out.println(args instanceof String[]);
	System.out.println("hello of Test");
    }
}

class Test2 extends Test {
    void hello(Object x) {
	System.out.println("hello of Test2");
    }
    void hello(Test x) {
	System.out.println("hello of Test2");
    }
}

class Dood {
    void hello() {
	System.out.println("hello of Dood");
    }

    interface DoodI {
	boolean hello();
    }
}
