package ru.ifmo.rain.tarasov.bank;

import java.io.Serializable;
import java.rmi.*;

public interface Account extends Serializable {
    /** Returns amount of money at the account. */
    int getAmount();

    /** Sets amount of money at the account. */
    void setAmount(int amount);
}