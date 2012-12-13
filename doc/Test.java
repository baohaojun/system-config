import java.util.*;
class Test {
    public static void main(String []args) {
	
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
