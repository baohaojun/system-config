package com.human;
import java.net.URI;
public class Child{
    public static void main(String[] args) {
        System.out.println(String.format("%s hello world: %s", "Child.java:4:", "what"));
        try {
            URI u = new URI("http", "", "localhost", 80, "/index.php", "hello=fuck world&what=hihi  you", "");
            System.out.println(String.format("%s u is %s", "Child.java:8:", u.toURL()));
        } catch (Exception e) {
        }
    }
}
