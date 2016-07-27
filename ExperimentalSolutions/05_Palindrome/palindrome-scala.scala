object Palindrome {

  def palindrome(a: String):Boolean = {
    var b:String = a.reverse
     b == a
  }

  def main(args: Array[String]) {
    println(palindrome("ABCD"))
    println(palindrome("ABCDCBA"))
  }
}