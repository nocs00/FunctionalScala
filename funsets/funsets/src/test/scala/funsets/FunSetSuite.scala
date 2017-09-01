package funsets

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    *  - test
    *  - ignore
    *  - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    * val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet(1) contains 1") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("betweenSet(1, 5) contains 1 and not contains 6") {
    val bs = betweenSet(1, 5)
    assert(contains(bs, 1), "Between set 1")
    assert(!contains(bs, 6), "Between set 2")
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only elements of both sets") {
    new TestSets {
      val s = intersect(betweenSet(0, 50), betweenSet(45, 60))
      assert(!contains(s, 44), "Intersect 1")
      assert(contains(s, 45), "Intersect 2")
      assert(contains(s, 50), "Intersect 3")
      assert(!contains(s, 51), "Intersect 4")
    }
  }

  test("diff contains only elements of left set which are not in right set") {
    val bs1 = betweenSet(1, 5)
    val bs2 = betweenSet(4, 10)
    val d = diff(bs1, bs2)

    assert(contains(d, 1), "Diff 1")
    assert(contains(d, 3), "Diff 2")
    assert(!contains(d, 4), "Diff 3")
    assert(!contains(d, 5), "Diff 4")
  }

  test("filter contains only elements of both sets") {
    val s = filter(betweenSet(0, 5), x => if (x == 0) true else false)
    assert(contains(s, 0), "Filter 1")
    assert(!contains(s, 1), "Filter 2")
    assert(!contains(s, 5), "Filter 4")
  }

  test("forall all integers within `s` satisfy `p`") {
    val s = betweenSet(0, 10)
    val p1 = betweenSet(-2000, 2000)
    val p2 = singletonSet(3)

    assert(forall(s, p1), "Forall 1")
    assert(!forall(s, p2), "Forall 2")
  }

  test("exists at least one integer within `s` satisfy `p`") {
    val s = betweenSet(0, 10)
    val p1 = betweenSet(11, 20)
    val p2 = singletonSet(3)

    assert(!exists(s, p1), "Forall 1")
    assert(exists(s, p2), "Forall 2")

    val evenAnd3 = (x: Int) => x % 2 == 0 || x == 3
    assert(exists(evenAnd3, x => x%2 == 1), "exists & filter: even and 3")
  }

  test("map: should apply function to each set member") {
    val set = betweenSet(1, 5)
    val mappedSet = map(set, x => x * 10)

    assert(contains(set, 3))
    assert(!contains(set, 30))
    assert(contains(mappedSet, 30))
    assert(!contains(mappedSet, 3))

    val set2 = listSet(List(1,3,4,5,7,1000))
    val mappedSet2 = map(set2, x => x - 1)
    val expectedSet = listSet(List(0,2,3,4,6,999))
    assert(forall(mappedSet2, expectedSet))

    val evenNumbersSet = (x: Int) => x % 2 == 0
    assert(forall(map(evenNumbersSet, (x) => x * 2), evenNumbersSet), "The set obtained by doubling all numbers should contain only even numbers.")
  }
}
