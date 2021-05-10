package msqueue;

import kotlinx.atomicfu.AtomicRef;



public class MSQueue implements Queue {
    private final AtomicRef<Node> head;
    private final AtomicRef<Node> tail;

    public MSQueue() {
        Node dummy = new Node(0);
        this.head = new AtomicRef<>(dummy);
        this.tail = new AtomicRef<>(dummy);
    }

    @Override
    public void enqueue(int x) {
        Node toBeAdded = new Node(x);
        while (true) {
            Node curTail = tail.getValue();
            if (curTail.next.compareAndSet(null, toBeAdded)) {
                tail.compareAndSet(curTail, toBeAdded);
                break;
            } else {
                tail.compareAndSet(curTail, curTail.next.getValue());
            }
        }
    }

    @Override
    public int dequeue() {
        while (true) {
            Node curHead = head.getValue();

            if (curHead.next.getValue() == null) {
                return Integer.MIN_VALUE;
            }

            // keeping invariant: tail is at least at head
            while (true) {
                Node curTail = tail.getValue();
                if (curHead == curTail) {
                    tail.compareAndSet(curTail, curTail.next.getValue());
                } else {
                    break;
                }
            }

            if (head.compareAndSet(curHead, curHead.next.getValue())) {
                return curHead.next.getValue().x;
            }
        }
    }

    @Override
    public int peek() {
        Node next = head.getValue()
                   .next.getValue();
        if (next == null)
            return Integer.MIN_VALUE;
        return next.x;
    }

    private static class Node {
        final int x;
        AtomicRef<Node> next;

        Node(int x) {
            this.x = x;
            next = new AtomicRef<>(null);
        }
    }
}