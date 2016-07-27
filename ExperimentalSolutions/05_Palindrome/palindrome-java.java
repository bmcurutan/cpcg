public class Palindrome {
    public Palindrome() {
    }
    public boolean palindrome(String a) {
        String b = new StringBuffer(a).reverse().toString();
        return a.equals(b);
    }
    /*public String reverse(String s) {
        String result = "";
        for (int i = 0; i < s.length(); i++) {
            result = s.charAt(i) + result;
        }
        return result;
    }*/
}