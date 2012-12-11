import java.util.*;
class Test {
    public static void main(String []args) {
	int[] xx = {2};

	char i = 0x8000;
	while (i != 0 && i == (char) -i) {
	    System.out.println("yes");
	}
	System.out.printf("%x\n", -i);


	System.out.println("2 + 2 = " + 2+2);
	System.out.println(xx instanceof int[]);
	System.out.println("hello world");
	Test x = new Test2();
	x.hello(x, args);
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
