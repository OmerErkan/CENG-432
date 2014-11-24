class IyteMutableSet(bucketsLength: Int) {

  private var buckets: Array[HashSetNode] = new Array[HashSetNode](bucketsLength)

  private var currentSize: Int = 0

  def contains(x: Int): Boolean = {
    var hashCode = x.hashCode
    if (hashCode < 0) {
      hashCode = -hashCode
    }
    hashCode = hashCode % buckets.length
    var current = buckets(hashCode)
    while (current != null) {
      if (current.data == x) {
        return true
      }
      current = current.next
    }
    false
  }

  def add(x: Int): Boolean = {
    var hashCode = x.hashCode
    if (hashCode < 0) {
    	hashCode = -hashCode
    }
    hashCode = hashCode % buckets.length
    var current = buckets(hashCode)
    while (current != null) {
      if (current.data == x) {
        return false
      }
      current = current.next
    }
    val newNode = new HashSetNode()
    newNode.data = x
    newNode.next = buckets(hashCode)
    buckets(hashCode) = newNode
    currentSize += 1
    true
  }
  override def toString(): String = {
    var result: String = "";
    val iter = iterator()
    while (iter.hasNext) {
      result = result + iter.next() + "\n"
    }
    result;
  }

  def iterator(): Iterator[Int] = new IyteMutableSetIterator()

  def size(): Int = currentSize

  class IyteMutableSetIterator extends Iterator[Int] {

    private var bucketIndex: Int = -1

    private var current: HashSetNode = null

    def hasNext(): Boolean = {
      if (current != null && current.next != null) {
        return true
      }
      for (b <- bucketIndex + 1 until buckets.length if buckets(b) != null) {
        return true
      }
      false
    }

    def next(): Int = {
      if (current != null && current.next != null) {
        current = current.next
      } else {
        do {
          bucketIndex += 1
          if (bucketIndex == buckets.length) {
            throw new NoSuchElementException()
          }
          current = buckets(bucketIndex)
        } while (current == null);
      }
      current.data
    }
  }

  object IyteMutableSet {
    def apply() = new IyteMutableSet(bucketsLength: Int);
  }

}

