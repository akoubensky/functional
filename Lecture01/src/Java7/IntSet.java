package Java7;
/**
 * �����, ����������� �������������� ������������� �������� �����.
 */
abstract class IntSet {
  // �������� �������������� �������� ���������
  abstract public boolean has(int elem);
  
  // ���������� �������� � ���������
  public static IntSet addElement(final IntSet s, final int n) {
    return new IntSet() {
      public boolean has(int elem) { return elem == n || s.has(elem); }
    };
  }
  
  // ���������� ��������� �� ��������� �����
  public static IntSet buildInterval(final int n, final int m) {
    return new IntSet() {
      public boolean has(int elem) { return elem >= n && elem <= m; }
    };
  }
  
  // ����������� ��������
  public static IntSet union(final IntSet s1, final IntSet s2) {
    return new IntSet() {
      public boolean has(int elem) { return s1.has(elem) || s2.has(elem); }
    };
  }
  
  // ����������� ��������
  public static IntSet intersection(final IntSet s1, final IntSet s2) {
    return new IntSet() {
      public boolean has(int elem) { return s1.has(elem) && s2.has(elem); }
    };
  }
  
  // �������� ��������
  public static IntSet difference(final IntSet s1, final IntSet s2) {
    return new IntSet() {
      public boolean has(int elem) { return s1.has(elem) && ! s2.has(elem); }
    };
  }
  
  // ������ ���������
  public static final IntSet emptySet = new IntSet() { public boolean has(int e) { return false; }};
  
  // �������� ����������������� ��������� ��������
  public static void main(String[] args) {
    IntSet oddSet = new IntSet() { public boolean has(int e) { return e % 2 == 1; }};
    System.out.println(difference(oddSet, addElement(emptySet, 3)).has(7));
  }
}
