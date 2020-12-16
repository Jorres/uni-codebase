package ru.ifmo.rain.tarasov.bank;

import java.io.Serializable;

public interface ILocalPerson extends Serializable {
    String getName();

    String getSurname();

    String getPassportId();

    int getAmount(final String accountId);

    int incrementAmount(final int change, final String accountId);
}
