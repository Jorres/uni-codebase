package queue;

public class ArrayQueue extends AbstractQueue implements Queue {
    private int STARTING_CAPACITY = 5;
    private int capacity = STARTING_CAPACITY, head = capacity - 1, tail = 0;
    private Object[] elements = new Object[STARTING_CAPACITY];

    public void doEnqueue(Object element) {
        ensureCapacity();
        elements[tail] = element;
        tail = (tail + 1) % capacity;
    }

    public Object doDequeue() {
        head = (head + 1) % capacity;
        return elements[head];
    }

    public Object element() {
        return elements[(head + 1) % capacity];
    }

    public void doClear() {
        tail = 0;
        head = STARTING_CAPACITY - 1;
        capacity = STARTING_CAPACITY;
        elements = new Object[STARTING_CAPACITY];
    }

    private void ensureCapacity() {
        if (tail == head && size > 0) {
            Object[] newElements = new Object[2 * capacity];
            for (int i = 0; i < size; i++) {
                newElements[i] = elements[(i + head + 1) % capacity];
            }

            capacity *= 2;
            head = capacity - 1;
            tail = size;
            elements = newElements;

        }
    }

    public ArrayQueue makeCopy() {
        ArrayQueue ans = new ArrayQueue();
        for (int i = 0; i < size; i++) {
            ans.enqueue(elements[(head + 1 + i) % capacity]);
        }
        return ans;
    }

    public void doToArray(Object[] elems) {
        for (int i = 0; i < size; i++) {
            elems[i] = elements[(head + 1 + i) % capacity];
        }
    }
}

