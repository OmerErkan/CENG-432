trait SymbolTable[K, V] {

  def get(key: String): String
  def set(key: String, value: String): Unit
  def checkSize(): Boolean
}
