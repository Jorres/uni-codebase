package ru.ifmo.rain.tarasov.concurrent;

import info.kgeorgiy.java.advanced.concurrent.AdvancedIP;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

/**
 * Implementation of {@link AdvancedIP} interface.
 */
public class IterativeParallelism implements AdvancedIP {
    private <FROM, TO> TO getProcessedList(int threads,
                                           final List<? extends FROM> elems,
                                           final Function<List<? extends FROM>, TO> processor,
                                           final BinaryOperator<TO> combiner)
                                                                throws InterruptedException {
        if (threads < 1) {
            throw new IllegalArgumentException("Amount of threads must be bigger than zero.");
        }

        final int n = elems.size();
        if (threads > n) {
            threads = n;
        }

        final Thread[] threadsContainer = new Thread[threads];
        final List<TO> processed = new ArrayList<>();
        for (int i = 0; i < threads; i++) {
            processed.add(null);
        }

        int sPoint = 0;
        int perThread = n / threads;
        for (int i = 0; i < threads; i++) {
            final int left = sPoint;
            final int right = ((i != threads - 1)
                    ? left + perThread - 1
                    : elems.size() - 1);

            sPoint = right + 1;

            final int savedIndex = i;
            threadsContainer[i] = new Thread(() -> {
                processed.set(savedIndex, processor.apply(elems.subList(left, right + 1)));
            });
            threadsContainer[i].start();
        }

        InterruptedException finalEx = null;
        for (int i = 0; i < threadsContainer.length; i++) {
            try {
                threadsContainer[i].join();
            } catch (InterruptedException ex) {
                if (finalEx == null) {
                    finalEx = new InterruptedException("Current thread was interrupted");
                    for (int j = i; j < threadsContainer.length; j++) {
                        threadsContainer[j].interrupt();
                    }
                }
            }
        }

        if (finalEx != null) {
            throw finalEx;
        }

        // sustain an invariant that optional is present
        return processed.stream().reduce(combiner).get();
    }

    /**
     * @param values list with input elements.
     * @throws IllegalArgumentException if threads is not positive.
     * @return input values reduced by monoid.
     */
    @Override
    public <T> T reduce(int threads, List<T> values, Monoid<T> monoid) throws InterruptedException {
        return mapReduce(threads, values, Function.identity(), monoid);
    }

    /**
     * @param values list with input elements.
     * @throws IllegalArgumentException if threads is not positive.
     */
    @Override
    public <T, R> R mapReduce(int threads, List<T> values, Function<T, R> f, Monoid<R> monoid)
            throws InterruptedException {

        return getProcessedList(threads, values,
                subl -> subl.stream().map(f).reduce(monoid.getIdentity(), monoid.getOperator()),
                monoid.getOperator());
    }

    /**
     * @param values list with input elements.
     * @throws IllegalArgumentException if threads is less then one.
     * @throws NoSuchElementException if input list is empty.
     * @return maximum of all input values.
     */
    @Override
    public <T> T maximum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
        return minimum(threads, values, Collections.reverseOrder(comparator));
    }

    /**
     * @param values list with input elements.
     * @throws IllegalArgumentException if threads is less then one.
     * @throws NoSuchElementException if values is empty.
     * @return minimum of all input values.
     */
    @Override
    public <T> T minimum(final int threads, final List<? extends T> values, final Comparator<? super T> comparator) throws InterruptedException {
        return getProcessedList(threads, values, subl -> subl.stream().min(comparator).get(), BinaryOperator.minBy(comparator));
    }

    /**
     * @param values list with input elements.
     * @throws IllegalArgumentException if threads is not positive.
     * @return bool equal to true, if predicate is true for all the values, false otherwise.
     */
    @Override
    public <T> boolean all(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
        return !any(threads, values, predicate.negate());
    }

    /**
     * @param values list with input elements.
     * @throws IllegalArgumentException if threads is not positive.
     * @return bool equal to true, if predicate is true for any of values, false otherwise.
     */
    @Override
    public <T> boolean any(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
        return getProcessedList(threads, values, s -> s.stream().anyMatch(predicate), (left, right) -> (left || right));
    }

    private StringBuilder stringAppender(StringBuilder l, StringBuilder r) {
        l.append(r.toString());
        return l;
    }

    /**
     * @param values list with input elements.
     * @return {@link #toString()} called on each element, joined into single {@link String}.
     * @throws IllegalArgumentException if threads is less then one.
     */
    @Override
    public String join(final int threads, final List<?> values) throws InterruptedException {
        return getProcessedList(threads, values, subl -> {
                    StringBuilder sb = new StringBuilder();
                    subl.forEach(sb::append);
                    return sb;
                },
                this::stringAppender).toString();
    }

    /**
     * @param values list with input elements.
     * @throws IllegalArgumentException if threads is less then one.
     * @return list of input values filtered by predicate.
     */
    @Override
    public <T> List<T> filter(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
        return getProcessedList(threads, values, subl -> subl.stream().filter(predicate).collect(Collectors.toList()),
                this::listCombiner);
    }

    private <T> List<T> listCombiner(List<T> f, List<T> s) {
        f.addAll(s);
        return f;
    }

    /**
     * @param values list with input elements.
     * @param f function to be applied to each element
     * @throws IllegalArgumentException if threads is less then one.
     * @return list with elements under mapping with f.
     */
    @Override
    public <T, U> List<U> map(final int threads, final List<? extends T> values, final Function<? super T, ? extends U> f) throws InterruptedException {
        return getProcessedList(threads, values, subl -> subl.stream().map(f).collect(Collectors.toList()),
                this::listCombiner);
    }
}
