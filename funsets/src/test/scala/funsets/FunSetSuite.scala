package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

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
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using .ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */

    test("singleton set one contains one".ignore) {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }


  def withinBounds: FunSet = (x: Int) => -1000 <= x && x <= 1000
  def withinDoubleBounds: FunSet = (x: Int) => 1000 <= x && x <= 2000
  def onlyNegatives: FunSet = (x: Int) => x <= 0
  def onlyPositives: FunSet = (x: Int) => x >= 0
  def positiveMapping: Int => Int = (x: Int) => -x
  def doubleMapping: Int => Int = (x: Int) => x * 2
  def strictlyLessThan5: FunSet = (x: Int) => x < 5

  test("forall within bound") {
    new TestSets:
      assert(forall(withinBounds, withinBounds), "All are within bounds")
      assert(forall(onlyNegatives, strictlyLessThan5), "All are within bounds")
  }

  test("negatives exists within bounds") {
    new TestSets:
      assert(exists(withinBounds, onlyNegatives), "Negatives exist within bounds")
  }

  test("map negatives to positives") {
    new TestSets:
      assert(forall(map(withinBounds, positiveMapping), onlyPositives), "Mapped withinbound set to contain only positive numbers")
      // assert(exists(withinDoubleBounds, map(withinBounds, doubleMapping)), "Mapped withinbound set to contain only x * x numbers")
  }



  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
