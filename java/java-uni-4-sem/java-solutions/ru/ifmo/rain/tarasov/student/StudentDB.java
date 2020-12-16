package ru.ifmo.rain.tarasov.student;

import info.kgeorgiy.java.advanced.student.AdvancedStudentGroupQuery;
import info.kgeorgiy.java.advanced.student.Group;
import info.kgeorgiy.java.advanced.student.Student;

import java.util.*;
import java.util.function.*;
import java.util.stream.*;

public class StudentDB implements AdvancedStudentGroupQuery {
    private final static Comparator<Student> FULL_COMPARATOR =
            Comparator.comparing(Student::getLastName)
                      .thenComparing(Student::getFirstName)
                      .thenComparing(Student::getId);

    @Override
    public List<Group> getGroupsByName(final Collection<Student> students) {
        return getGroups(students, FULL_COMPARATOR);
    }

    @Override
    public List<Group> getGroupsById(final Collection<Student> students) {
        return getGroups(students, Comparator.comparingInt(Student::getId));
    }

    @Override
    public String getLargestGroup(final Collection<Student> students) {
        return maxEntryKeyForComparator(enlist(groupByGetGroup(students)),
                                        Comparator.reverseOrder(),
                                        List::size);
    }

    @Override
    public String getLargestGroupFirstName(final Collection<Student> students) {
        /*return students.stream()
                .collect(Collectors.groupingBy(Student::getGroup, Collectors.counting())).entrySet().stream()
                .max(Map.Entry.<String, Long>comparingByValue()
                        .thenComparing(Map.Entry.comparingByKey(Comparator.reverseOrder())))
                .map(Map.Entry::getKey)
                .orElse(""); WA 24 (((( */
        return maxEntryKeyForComparator(enlist(groupByGetGroup(students)),
                Comparator.reverseOrder(),
            list -> getDistinctFirstNames(list).size());
    }

    @Override
    public List<String> getFirstNames(final List<Student> students) {
        return customMap(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(final List<Student> students) {
        return customMap(students, Student::getLastName);
    }

    @Override
    public List<String> getGroups(final List<Student> students) {
        return customMap(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(final List<Student> students) {
        return customMap(students, this::getStudentFullName);
    }

    @Override
    public Set<String> getDistinctFirstNames(final List<Student> students) {
        return getDistinctValues(students, Student::getFirstName);
    }

    @Override
    public String getMinStudentFirstName(final List<Student> students) {
        return students.stream()
                .min(Comparator.comparingInt(Student::getId))
                .map(Student::getFirstName)
                .orElse("");
    }

    @Override
    public List<Student> sortStudentsById(final Collection<Student> students) {
        return sortByGivenComparator(students, Comparator.comparingInt(Student::getId));
    }

    @Override
    public List<Student> sortStudentsByName(final Collection<Student> students) {
        return sortByGivenComparator(students, FULL_COMPARATOR);
    }

    @Override
    public List<Student> findStudentsByFirstName(final Collection<Student> students, String name) {
        return customFilterToString(students, Student::getFirstName, name);
    }

    @Override
    public List<Student> findStudentsByLastName(final Collection<Student> students, String name) {
        return customFilterToString(students, Student::getLastName, name);
    }

    @Override
    public List<Student> findStudentsByGroup(final Collection<Student> students, String group) {
        return customFilterToString(students, Student::getGroup, group);
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(final Collection<Student> students, String group) {
        return findStudentsByGroup(students, group).stream().collect(Collectors.toMap(
                                                                Student::getLastName,
                                                                Student::getFirstName,
                                                                BinaryOperator.minBy(String::compareTo)));
    }

    @Override
    public String getMostPopularName(final Collection<Student> students) {
        return maxEntryKeyForComparator(
                enlist(group(students, this::getStudentFullName)),
                String::compareTo,
                list -> {
                    return getDistinctValues(list, Student::getGroup).size();
                });
    }

    @Override
    public List<String> getFirstNames(final Collection<Student> students, int[] indices) {
        return getByIndicesChecking(students, indices, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(final Collection<Student> students, int[] indices) {
        return getByIndicesChecking(students, indices, Student::getLastName);
    }

    @Override
    public List<String> getFullNames(final Collection<Student> students, int[] indices) {
        return getByIndicesChecking(students, indices, this::getStudentFullName);
    }

    @Override
    public List<String> getGroups(final Collection<Student> students, int[] indices) {
        return getByIndicesChecking(students, indices, Student::getGroup);
    }

    private String getStudentFullName(final Student student) {
        return student.getFirstName() + " " + student.getLastName();
    }

    private static List<String> customMap(final List<Student> students, Function<Student, String> function) {
        return enlist(students.stream().map(function));
    }

    private static <T> List<T> enlist(Stream<T> s) {
        return s.collect(Collectors.toList());
    }

    private static List<Student> sortByGivenComparator(final Collection<Student> coll, Comparator<Student> comparator) {
        return enlist(coll.stream().sorted(comparator));
    }

    private static List<Student> customFilterToString(final Collection<Student> students,
                                                      Function<Student, String> getValue,
                                                      String expectedValue) {
        return sortByGivenComparator(enlist(students.stream()
                .filter(s -> getValue.apply(s).equals(expectedValue))),
                FULL_COMPARATOR);
    }

    private static Stream<Map.Entry<String, List<Student>>> groupByGetGroup(final Collection<Student> students) {
        return group(students, Student::getGroup);
    }

    private static Stream<Map.Entry<String, List<Student>>> group(final Collection<Student> students,
                                                                  Function<Student, String> getter) {
        return students.stream()
                .collect(Collectors.groupingBy(
                        getter,
                        TreeMap::new,
                        Collectors.toList()))
                .entrySet()
                .stream();
    }

    private static Group getSortedGroup(final Map.Entry<String, List<Student>> e, Comparator<Student> order) {
        return new Group(e.getKey(), sortByGivenComparator(e.getValue(), order));
    }

    private static List<Group> getGroups(final Collection<Student> students, Comparator<Student> order) {
        return groupByGetGroup(students)
                .map(e -> getSortedGroup(e, order)).collect(Collectors.toList());
    }

    // returns the key that is maximal via a comparator
    private static String maxEntryKeyForComparator(final List<Map.Entry<String, List<Student>>> list,
                                                   Comparator<String> keyComparator,
                                                   ToIntFunction<List<Student>> valueKeyComparator) {
        return list.stream()
                .max(Comparator.comparingInt((Map.Entry<String, List<Student>> g) -> valueKeyComparator.applyAsInt(g.getValue()))
                    .thenComparing(getKey(), keyComparator))
                .map(Map.Entry::getKey).orElse("");
    }

    private List<String> getByIndicesChecking(final Collection<Student> students,
                                              int[] indices,
                                              Function<Student, String> getValue) {
        // FIX LATER
        if (students instanceof ArrayList) {
            return getFromIndices((ArrayList<Student>)students, indices, getValue);
        } else {
            return getFromIndices(new ArrayList<>(students), indices, getValue);
        }
    }

    private List<String> getFromIndices(final List<Student> students, int[] indices,
                                        Function<Student, String> getValue) {
        return enlist(Arrays.stream(indices)
                .boxed()
                .map(students::get)
                .map(getValue));
    }

    private static <K, V> Function<Map.Entry<K, V>, K> getKey() {
        return Map.Entry::getKey;
    }

    private static Set<String> getDistinctValues(final List<Student> students, Function<Student, String> getValue) {
        return students.stream().map(getValue).collect(Collectors.toSet());
    }
}