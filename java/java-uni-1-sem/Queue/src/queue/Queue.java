package queue;

import java.util.function.Function;
import java.util.function.Predicate;

// inv: size == 0 || elems[1..size] are valid
public interface Queue {

    // pre: elem != null
    // post: last element in queue is elem && inv
    void enqueue(Object elem);

    // pre: size > 0
    // post: first element from queue is removed and returned && inv
    Object dequeue();

    // pre: size > 0
    // post: first element from queue is returned && inv
    Object element();

    // pre: none
    // post: none change is made, queue size returned
    int size();

    // pre: none
    // post: none change is made, returns true if size == 0, false otherwise
    boolean isEmpty();

    // pre: none
    // post: queue is clear and can be used as if recently created
    void clear();

    // pre: none
    // post: array [0..size - 1] with valid queue elements is returned
    Object[] toArray();

    // pre: none
    // post: returns a queue with elements, satisfying the predicate, in the same order as in the initial
    Queue filter(Predicate<Object> p);

    // pre: none
    // post: returns a queue with elements which resulted from applying function to elements of initial queue
    // elements follow in the same order as in the initial queue
    Queue map(Function<Object, Object> f);
};
