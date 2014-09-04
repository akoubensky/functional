package Java8;

import java.util.function.Predicate;

@FunctionalInterface
public interface IntSet extends Predicate<Integer> {
	
	/**
	 * ������ ���������
	 */
	public static IntSet EMPTY_SET = x -> false;
	
	/**
	 * ��������� �����, ��������� �������������� ���������.
	 * 
	 * @param n	������� - ���������� ���������
	 * @return	���������, ���������� ������������ ������� - ��������.
	 */
	public static IntSet of(int n) {
		return e -> e == n;
	}

	/**
	 * ��������� ����� ������� � ���������.
	 * 
	 * @param set	���������, � ������� ����������� �������
	 * @param elem	����������� �������
	 * @return		����� ����������� ���������
	 */
	public static IntSet addElement(IntSet set, int elem) {
		return n -> n == elem || set.test(n);
	}
	
	/**
	 * ������ ����� ���������, ���������� ����� ����� �� ��������� ���������.
	 * 
	 * @param low	������ ������� ��������� (����������)
	 * @param high	������� ������� ��������� (�����������)
	 * @return		����������� ���������
	 */
	public static IntSet build(int low, int high) {
		return n -> n >= low && n < high;
	}
	
	/**
	 * ������ ����� ��������� ��� ����������� ���� �������� ��������.
	 * 
	 * @param s1	������ ���������
	 * @param s2	������ ���������
	 * @return		���������, ���������� ��� �������� ������� � ������� ��������
	 */
	public static IntSet union(IntSet s1, IntSet s2) {
		return n -> s1.test(n) || s2.test(n);
	}
	
	/**
	 * ������ ����� ��������� ��� ����������� ���� �������� ��������.
	 * 
	 * @param s1	������ ���������
	 * @param s2	������ ���������
	 * @return		���������, ���������� ��������, ������������ ��� � ������,
	 * 				��� � �� ������ ���������
	 */
	public static IntSet intersection(IntSet s1, IntSet s2) {
		return n -> s1.test(n) && s2.test(n);
	}
	
	/**
	 * ��������� �������� ���� �������� ��������.
	 * 
	 * @param s1	������ ���������
	 * @param s2	������ ���������
	 * @return		���������, ���������� ��� �������� ������� ���������,
	 * 				������� �� �������� ���������� ������� ���������
	 */
	public static IntSet difference(IntSet s1, IntSet s2) {
		return n -> s1.test(n) && !s2.test(n);
	}
	
	/**
	 * �������� ������� ��� �������� ����������������� ��������� �������.
	 * @param args
	 */
	public static void main(String... args) {
		System.out.println("Start...");
		IntSet oddSet = n -> (n & 1) == 1;
		System.out.println(difference(oddSet, addElement(EMPTY_SET, 3)).test(7));
	}
}
