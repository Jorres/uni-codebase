package queue;

import java.util.function.Function;
import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue {
    protected int size = 0;

    public Object dequeue() {
        assert size > 0;
        size--;
        return doDequeue();
    }

    public void enqueue(Object elem) {
        assert elem != null;
        doEnqueue(elem);
        size++;
    }

    public int size() {
        return size;
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public void clear() {
        size = 0;
        doClear();
    }

    public Object[] toArray() {
        Object[] ans = new Object[size];
        doToArray(ans);
        return ans;
    }

    public AbstractQueue filter(Predicate<Object> p) {
        AbstractQueue q = makeCopy();

        int sz = q.size;
        for (int i = 0; i < sz; i++) {
            if (p.test(q.element())) {
                q.enqueue(q.dequeue());
            } else {
                q.dequeue();
            }
        }
        return q;
    }

    public AbstractQueue map(Function<Object, Object> f) {
        AbstractQueue q = makeCopy();
        int sz = q.size;
        for (int i = 0; i < sz; i++) {
            q.enqueue(f.apply(q.element()));
            q.dequeue();
        }
        return q;
    }

    public abstract AbstractQueue makeCopy();
    public abstract Object element();
    public abstract void doClear();
    public abstract Object doDequeue();
    public abstract void doEnqueue(Object elem);
    public abstract void doToArray(Object[] ans);
}
