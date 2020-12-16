package queue;

public class LinkedQueue extends AbstractQueue implements Queue {
    public class Node {
        Object value;
        Node next;
        // inv: value != null
        public Node(Object value, Node next) {
            assert value != null;
            this.value = value;
            this.next = next;
        }
    }

    private Node head = null, tail = null;

    public void doEnqueue(Object element) {
        if (size == 0) {
            head = new Node(element, null);
            tail = head;
        } else {
            Node t = new Node(element, null);
            tail.next = t;
            tail = t;
        }
    }

    public Object doDequeue() {
        Object t = head.value;
        head = head.next;
        return t;
    }

    public Object element() {
        return head.value;
    }

    public void doClear() {
        while (head != null) {
            head = head.next;
        }
    }

    public LinkedQueue makeCopy() {
        LinkedQueue ans = new LinkedQueue();
        for (Node t = head; t != null; t = t.next) {
            ans.enqueue(t.value);
        }
        return ans;
    }

    public void doToArray(Object[] elems) {
        Node t = head;
        int i = 0;
        for (; t != null; t = t.next) {
            elems[i++] = t.value;
        }
    }
}
