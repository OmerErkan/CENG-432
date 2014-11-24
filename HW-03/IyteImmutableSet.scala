class IyteImmutableSet {
  private var buckets: Array[HashSetNode] = new Array[HashSetNode](100)

  private var currentSize: Int = 0
  def this(x: Int) {
    this();
    addItem(x)
    

  }
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

  def add(x: Int): IyteImmutableSet = {
    new IyteImmutableSet(x);
  }

  
  override def toString(): String = {
    var result: String = "";
    val iter = iterator()
    while (iter.hasNext) {
      result = result + iter.next() + "\n"
    }
    result;
  }
def addItem(x: Int): Boolean = {
    var result: String = ""
    var hashCode = x.hashCode
    if (hashCode < 0) {
      hashCode = -hashCode
    }
    hashCode = hashCode % buckets.length
    var current = buckets(hashCode)
    val iter = iterator()

    while (iter.hasNext) {
      if (iter.next()== x) {
        return false;
      }
      current = current.next
    }
    val newNode = new HashSetNode()
    newNode.data = x
    newNode.next = buckets(hashCode)
    buckets(hashCode) = newNode
    currentSize += 1
    printf(toString())
    true
  }
  def iterator(): Iterator[Int] = new IyteImmutableSetSetIterator()

  def size(): Int = currentSize

  class IyteImmutableSetSetIterator extends Iterator[Int] {

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

  object IyteImmutableSet {
    def apply() = new IyteImmutableSet();
  }

}

