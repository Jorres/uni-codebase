package ru.ifmo.rain.tarasov.arrayset;

import java.util.*;

public class ArraySet<T> extends AbstractSet<T> implements NavigableSet<T>  {

    private Comparator<? super T> givenComparator;
    private Comparator<? super T> actualComparator;

    private static class BSearchSetting {
        private final int isFound;
        private final boolean isBigger;

        public BSearchSetting(int isFound, boolean isBigger) {
            this.isFound = isFound;
            this.isBigger = isBigger;
        }
    }

    private static final BSearchSetting ceilingSetting = new BSearchSetting(0, false);
    private static final BSearchSetting floorSetting = new BSearchSetting(0, true);

    private static final BSearchSetting higherSetting = new BSearchSetting(1, false);
    private static final BSearchSetting lowerSetting = new BSearchSetting(-1, true);

    private final List<T> data;

    private ArraySet(List<T> sorted) {
        data = sorted;
    }

    private ArraySet(List<T> sorted, Comparator<? super T> given, Comparator<? super T> actual) {
        data = sorted;
        givenComparator = given;
        actualComparator = actual;
    }

    @SuppressWarnings("unchecked")
    private Comparator<? super T> resolveActualComparator(Comparator<? super T> cmp) {
        return cmp == null
                ? (Comparator<? super T>)Comparator.naturalOrder()
                : cmp;
    }

    public ArraySet() {
        data = Collections.emptyList();
        givenComparator = null;
        actualComparator = null;
    }

    public ArraySet(Collection<? extends T> collection) {
        this(collection, null);
    }

    public ArraySet(Collection<? extends T> collection, Comparator<? super T> comparator) {
        this(makeUnique(collection, comparator));
        this.givenComparator = comparator;
        this.actualComparator = resolveActualComparator(comparator);
    }

    static <T> List<T> makeUnique(Collection<? extends T> collection, Comparator<? super T> cmp) {
        TreeSet<T> sorted = new TreeSet<>(cmp);
        collection.forEach(sorted::add);
        return new ArrayList<>(sorted);
    }

    private T getByIndex(int index) {
        return (0 > index || index >= data.size()) ? null : data.get(index);
    }

    private int binarySearch(T o, BSearchSetting settings) {
        int index = Collections.binarySearch(data, o, actualComparator);
        int updated;
        if (index < 0) {
            int actual = -index - 1;
            updated = actual  + (settings.isBigger ? -1 : 0);
        } else {
            updated = index + settings.isFound;
        }

        if (0 <= updated && updated < data.size()) {
            return updated;
        } else {
            return -1;
        }
    }

    @Override
    public T lower(T o) {
        return (data.isEmpty() ? null : getByIndex(binarySearch(o, lowerSetting)));
    }

    @Override
    public T floor(T o) {
        return (data.isEmpty() ? null : getByIndex(binarySearch(o, floorSetting)));
    }

    @Override
    public T ceiling(T o) {
        return (data.isEmpty() ? null : getByIndex(binarySearch(o, ceilingSetting)));
    }

    @Override
    public T higher(T o) {
        return (data.isEmpty() ? null : getByIndex(binarySearch(o, higherSetting)));
    }

    @Override
    public T pollFirst() {
        throw new UnsupportedOperationException("pollFirst is not supported on immutable set");
    }

    @Override
    public T pollLast() {
        throw new UnsupportedOperationException("pollLast is not supported on immutable set");
    }

    @Override
    public Iterator<T> iterator() {
        return Collections.unmodifiableList(data).iterator();
    }

    @Override
    public NavigableSet<T> descendingSet() {
        return new ArraySet<>(ReversedList.reverseList(data), givenComparator, actualComparator.reversed());
    }

    @Override
    public Iterator<T> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public NavigableSet<T> headSet(T to, boolean inclusive) {
        return (data.isEmpty() ? fabricateEmptySet() : subSet(first(), true, to, inclusive));
    }

    private NavigableSet<T> fabricateEmptySet() {
        return new ArraySet<>(Collections.emptyList(), givenComparator, actualComparator);
    }

    @Override
    public NavigableSet<T> tailSet(T from, boolean inclusive) {
        return (data.isEmpty() ? fabricateEmptySet() : subSet(from, inclusive, last(), true));
    }

    @Override
    public SortedSet<T> subSet(T from, T to) {
        if (actualComparator.compare(from, to) > 0) {
            throw new IllegalArgumentException("left element is bigger than right one");
        }
        return subSet(from, true, to, false);
    }

    @Override
    public SortedSet<T> headSet(T toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<T> tailSet(T fromElement) {
        return tailSet(fromElement, true);
    }

    @Override
    public NavigableSet<T> subSet(T from, boolean includeLeft, T to, boolean includeRight) {
        int left = includeLeft
                ? binarySearch(from, ceilingSetting)
                : binarySearch(from, higherSetting);
        int right = includeRight
                ? binarySearch(to, floorSetting)
                : binarySearch(to, lowerSetting);

        if (left > right || left == -1 || right == -1) {
            return fabricateEmptySet();
        }

        return new ArraySet<>(data.subList(left, right + 1), givenComparator, actualComparator);
    }

    @Override
    public Comparator<? super T> comparator() {
        if (givenComparator == Comparator.naturalOrder()) {
            return null;
        }
        return givenComparator;
    }

    private void checkForEmpty(String message) {
        if (data.size() == 0) {
            throw new NoSuchElementException(message);
        }
    }

    @Override
    public T first() {
        checkForEmpty("first is called on empty set");
        return data.get(0);
    }

    @Override
    public T last() {
        checkForEmpty("last is called on empty set");
        return data.get(size() - 1);
    }

    @Override
    public int size() {
        return data.size();
    }

    @Override
    @SuppressWarnings("unchecked")
    public boolean contains(Object o) {
        return Collections.binarySearch(data, (T)o, actualComparator) >= 0;
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException("clear is not allowed on immutable set");
    }
}
