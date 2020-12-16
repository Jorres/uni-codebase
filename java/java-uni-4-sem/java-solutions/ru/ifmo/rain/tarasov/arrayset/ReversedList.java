package ru.ifmo.rain.tarasov.arrayset;

import java.util.AbstractList;
import java.util.List;

public class ReversedList<T> extends AbstractList<T> {
    private List<T> data;

    private ReversedList(List<T> data) {
        this.data = data;
    }

    public static <T> List<T> reverseList(List<T> data) {
        if (data instanceof ReversedList) {
            return ((ReversedList<T>) data).data;
        }
        return new ReversedList<>(data);
    }

    @Override
    public T get(int i) {
        return data.get(data.size() - i - 1);
    }

    @Override
    public int size() {
        return data.size();
    }
}
