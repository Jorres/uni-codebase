package ru.ifmo.rain.tarasov.bank;

public class RequestContainer {
    final String name;
    final String surname;
    final String passportId;
    final String accountId;
    final int change;

    public RequestContainer(String name, String surname, String passportId, String accountId, int change) {
        this.name = name;
        this.surname = surname;
        this.passportId = passportId;
        this.accountId = accountId;
        this.change = change;
    }
};