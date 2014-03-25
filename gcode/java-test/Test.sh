echo 'class Foo {'\
 'public static void main(String[] args) {'\
 'System.out.println("Hello, world"); }}' > Foo.java
 javac Foo.java
 dx --dex --output=foo.jar Foo.class
 adb push foo.jar /sdcard/
 adb shell dalvikvm -cp /sdcard/foo.jar Foo
