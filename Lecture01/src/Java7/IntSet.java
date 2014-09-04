package Java7;
/**
 * Класс, реализующий функциональное представление множеств целых.
 */
abstract class IntSet {
  // Проверка принадлежности элемента множеству
  abstract public boolean has(int elem);
  
  // Добавление элемента в множество
  public static IntSet addElement(final IntSet s, final int n) {
    return new IntSet() {
      public boolean has(int elem) { return elem == n || s.has(elem); }
    };
  }
  
  // Построение множества из диапазона чисел
  public static IntSet buildInterval(final int n, final int m) {
    return new IntSet() {
      public boolean has(int elem) { return elem >= n && elem <= m; }
    };
  }
  
  // Объединение множеств
  public static IntSet union(final IntSet s1, final IntSet s2) {
    return new IntSet() {
      public boolean has(int elem) { return s1.has(elem) || s2.has(elem); }
    };
  }
  
  // Пересечение множеств
  public static IntSet intersection(final IntSet s1, final IntSet s2) {
    return new IntSet() {
      public boolean has(int elem) { return s1.has(elem) && s2.has(elem); }
    };
  }
  
  // Разность множеств
  public static IntSet difference(final IntSet s1, final IntSet s2) {
    return new IntSet() {
      public boolean has(int elem) { return s1.has(elem) && ! s2.has(elem); }
    };
  }
  
  // Пустое множество
  public static final IntSet emptySet = new IntSet() { public boolean has(int e) { return false; }};
  
  // Проверка работоспособности некоторых операций
  public static void main(String[] args) {
    IntSet oddSet = new IntSet() { public boolean has(int e) { return e % 2 == 1; }};
    System.out.println(difference(oddSet, addElement(emptySet, 3)).has(7));
  }
}
