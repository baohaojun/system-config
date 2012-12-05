import java.util.*;
class Test {
    public static void main(String []args) {
	System.out.println("hello world");
	Test x = new Test2();
	x.hello(x);
    }
    void hello(Test x) {
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
