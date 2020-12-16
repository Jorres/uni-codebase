package ru.ifmo.rain.tarasov.walk;

import java.io.*;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;

public class FileWalker extends SimpleFileVisitor<Path> {
    static final int HASH_ON_ERROR = 0;

    static final int FNV_PRIME = 0x01000193;
    static final int STARTING_FNV_VALUE = 0x811c9dc5;

    static final int BUF_SIZE = 1024;

    private final byte[] buffer;
    private final Writer outputFileWriter;

    FileWalker(Writer fileWriter) {
        buffer = new byte[BUF_SIZE];
        this.outputFileWriter = fileWriter;
    }

    private int fnvHash(InputStream inputStream) throws IOException {
        int hash = STARTING_FNV_VALUE;
        int elemsRead;

        while (true) {
            elemsRead = inputStream.read(buffer);
            if (elemsRead < 0) {
                break;
            }

            for (int i = 0; i < elemsRead; i++) {
                hash *= FNV_PRIME;
                hash ^= Byte.toUnsignedInt(buffer[i]);
            }
        }

        return hash;
    }

    @Override
    public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) throws IOException {
        int hash;

        try (InputStream stream = Files.newInputStream(path)) {
            hash = fnvHash(stream);
        } catch (IOException e) {
            hash = HASH_ON_ERROR;
        }

        printHash(path.toString(), hash);
        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult visitFileFailed(Path path, IOException exc) throws IOException {
        printHash(path.toString(), HASH_ON_ERROR);
        return FileVisitResult.CONTINUE;
    }

    void printHash(String pathName, int hash) throws IOException {
        outputFileWriter.write(String.format("%08x %s%n", hash, pathName));
    }
}