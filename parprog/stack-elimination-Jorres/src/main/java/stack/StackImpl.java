package stack;

import kotlinx.atomicfu.AtomicInt;
import kotlinx.atomicfu.AtomicLong;
import kotlinx.atomicfu.AtomicRef;

import java.util.concurrent.ThreadLocalRandom;

public class StackImpl implements Stack {
    private static final int ELIM_SIZE = 10;
    private static final int SEQUENCE_TRY_LENGTH = 10;

    private static final int SPIN_LOOP = 100;
    private static final long ABSENSE = Integer.MIN_VALUE - 1L;

    private static class Node {
        final AtomicRef<Node> next;
        final int x;

        Node(int x, Node next) {
            this.next = new AtomicRef<>(next);
            this.x = x;
        }
    }

    public StackImpl() {
         elim = new AtomicLong[ELIM_SIZE];
         for (int i = 0; i < ELIM_SIZE; i++) {
             elim[i] = new AtomicLong(ABSENSE);
         }
         head = new AtomicRef<>(null);
    }

    private final AtomicRef<Node> head;
    private final AtomicLong[] elim;

    @Override
    public void push(int x) {
        basePush(x);
        // int pos = getRandBorders();
        //
        // for (int i = 0; i < SEQUENCE_TRY_LENGTH; i++) {
        //     int curPos = (i + pos) % ELIM_SIZE;
        //     if (elim[curPos].compareAndSet(ABSENSE, x)) {
        //         for (int j = 0; ; j++) {
        //             if (elim[curPos].getValue() == ABSENSE) {
        //                 return;
        //             }
        //
        //             if (j >= SPIN_LOOP) {
        //                 if (elim[curPos].compareAndSet(x, ABSENSE)) {
        //                     basePush(x);
        //                     return;
        //                 }
        //             }
        //         }
        //     }
        // }
        // basePush(x);
    }

    private void basePush(int x) {
        while (true) {
            Node curHead = head.getValue();
            Node added = new Node(x, curHead);
            if (head.compareAndSet(curHead, added)) {
                return;
            }
        }
    }

    @Override
    public int pop() {
        return basePop();
        // int pos = getRandBorders();
        //
        // for (int i = 0; i < SEQUENCE_TRY_LENGTH; i++) {
        //     int curPos = (i + pos) % ELIM_SIZE;
        //     long intVal = elim[curPos].getValue();
        //     if (intVal != ABSENSE) {
        //         if (elim[curPos].compareAndSet(intVal, ABSENSE)) {
        //             return (int)intVal;
        //         }
        //     }
        // }
        // return basePop();
    }

    private int basePop() {
        while (true) {
            Node curHead = head.getValue();
            if (curHead == null) {
                return Integer.MIN_VALUE;
            }
            if (head.compareAndSet(curHead, curHead.next.getValue())) {
                return curHead.x;
            }
        }
    }

    private int getRandBorders() {
        return ThreadLocalRandom
                .current()
                .nextInt(0, ELIM_SIZE);
    }
}
