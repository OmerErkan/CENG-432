
class IyteHashTable[K, V](private var maxSize: Double, private var minSize: Double, private var set: Double) 
extends SymbolTable[K,V]{
  private val MIN_CAP = 11
  var size: Int = 0
  private var capacity: Int = MIN_CAP
  private var elements: Array[Pair] = Array.ofDim[Pair](capacity)

  assert(set < maxSize && maxSize < 1)

  assert(0 < minSize && minSize < set)

  def this(maxSize: Double, minSize: Double) {
    this(maxSize, minSize, 0.5)
  }
  def this() {
    this(0.75, 0.25)
  }

  def set(key: String, `val`: String) {
    assert(key != null)
    var i = hashing(key) % capacity
    while (elements(i) != null && key != elements(i).k) {
      i = (i + 1) % capacity

    }
    if (elements(i) == null)
      size += 1
    elements(i) = new Pair(key, `val`)

    //println("key :" + elements(i).k + "\tvalue : " + elements(i).v)
    resizeIfNeeded()
  }

  def get(key: String): String = {
    assert(key != null)
    var result: String = "NullPointerException "
    var i = hashing(key) % capacity

    while (key != elements(i).k) {
      i = (i + 1) % capacity
    }
    if (elements(i) != null) {
      result = elements(i).v
    }

    result

  }

  private def resizeIfNeeded() {
    if (!((size < capacity * minSize && capacity > MIN_CAP) || size > capacity * maxSize)) {
      return
    }
    var newCapacity = (size / set).toInt
    val newItems: Array[Pair] = Array.ofDim[Pair](newCapacity)

    for (j <- 0 until capacity) {

      val q: Pair = elements(j)
      if (q == null) {

      }
      var i = hashing(q.k) % newCapacity
      while (newItems(i) != null && q.k != newItems(i).k) {
        i = (i + 1) % newCapacity
      }
      newItems(i) = q
    }
    this.elements = newItems
    this.capacity = newCapacity
  }

  def checkSize(): Boolean = {
    var x = 0
    for (i <- 0 until capacity if elements(i) != null) x += 1
    x == size
  }

  def hashing(s: String): Int = {
    s.charAt(0) % 5
  }
  class Pair(var k: String, var v: String) {

    def Pair(k: String, v: String) {
      var value: String = v
      var key: String = k
    }

  }

  object IyteHashTable {
    def apply() = new IyteHashTable();
  }
}
