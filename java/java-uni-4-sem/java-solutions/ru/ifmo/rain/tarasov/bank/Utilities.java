package ru.ifmo.rain.tarasov.bank;

import java.util.Objects;

public class Utilities {
    public static String getRegistryAddress(final int registryPort, final String objectName) {
        return "//localhost:" + registryPort + "/" + objectName;
    }

    public static void assertClientArgumentsNonNull(String[] args) {
        Objects.requireNonNull(args, "Arguments are null.\nRequired - name surname passport_num account_num change");
        for (int i = 0; i < 5; i++) {
            Objects.requireNonNull(args[i], "Required argument is missing, check your input." +
                    "\nRequired - name surname passport_num account_num change");
        }
    }
}
