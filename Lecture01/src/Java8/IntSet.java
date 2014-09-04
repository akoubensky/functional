package Java8;

import java.util.function.Predicate;

@FunctionalInterface
public interface IntSet extends Predicate<Integer> {
	
	/**
	 * Пустое множество
	 */
	public static IntSet EMPTY_SET = x -> false;
	
	/**
	 * Фабричный метод, создающий одноэлементное множество.
	 * 
	 * @param n	Элемент - содержимое множества
	 * @return	Множество, содержащее единственный элемент - аргумент.
	 */
	public static IntSet of(int n) {
		return e -> e == n;
	}

	/**
	 * Добавляет новый элемент в множество.
	 * 
	 * @param set	Множество, в которое добавляется эдемент
	 * @param elem	Добавляемый элемент
	 * @return		Новое построенное множество
	 */
	public static IntSet addElement(IntSet set, int elem) {
		return n -> n == elem || set.test(n);
	}
	
	/**
	 * Строит новое множество, содержащее целые числа из заданного интервала.
	 * 
	 * @param low	Нижняя граница интервала (включается)
	 * @param high	Верхняя граница интервала (исключается)
	 * @return		Построенное множество
	 */
	public static IntSet build(int low, int high) {
		return n -> n >= low && n < high;
	}
	
	/**
	 * Строит новое множество как объединение двух заданных множеств.
	 * 
	 * @param s1	Первое множество
	 * @param s2	Второе множество
	 * @return		Множество, содержащие все элементы первого и второго множеств
	 */
	public static IntSet union(IntSet s1, IntSet s2) {
		return n -> s1.test(n) || s2.test(n);
	}
	
	/**
	 * Строит новое множество как пересечение двух заданных множеств.
	 * 
	 * @param s1	Первое множество
	 * @param s2	Второе множество
	 * @return		Множество, содержащие элементы, содержащиеся как в первом,
	 * 				так и во втором множестве
	 */
	public static IntSet intersection(IntSet s1, IntSet s2) {
		return n -> s1.test(n) && s2.test(n);
	}
	
	/**
	 * Вычисляет разность двух заданных множеств.
	 * 
	 * @param s1	Первое множество
	 * @param s2	Второе множество
	 * @return		Множество, содержащие все элементы первого множества,
	 * 				которые не являются элементами второго множества
	 */
	public static IntSet difference(IntSet s1, IntSet s2) {
		return n -> s1.test(n) && !s2.test(n);
	}
	
	/**
	 * Тестовая функция для проверки работоспособности некоторых функций.
	 * @param args
	 */
	public static void main(String... args) {
		System.out.println("Start...");
		IntSet oddSet = n -> (n & 1) == 1;
		System.out.println(difference(oddSet, addElement(EMPTY_SET, 3)).test(7));
	}
}
