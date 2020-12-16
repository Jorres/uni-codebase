package ru.ifmo.rain.tarasov.walk;

class FileWalkerException extends Exception {
    FileWalkerException(String message, Exception e) {
        super(message + ": " + e.getMessage(), e);
    }
}