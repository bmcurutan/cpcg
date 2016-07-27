public class Palindrome {
    public boolean palindrome(String a) {
        return a.equals(new StringBuffer(a).reverse().toString());
    }
}