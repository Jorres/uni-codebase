package ru.ifmo.rain.tarasov.walk;


import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;

public class RecursiveWalk {
    public static void main(String[] args) {
        if (args == null) {
            System.out.println("Specify non-null arguments list");
            return;
        }

        if (args.length != 2 || args[0] == null || args[1] == null) {
            System.out.println("Exactly two arguments expected, input_file, output_file");
            return;
        }
            
        String inputFile = args[0];
        String outputFile = args[1];

        try {
            Path outputPath;
            try {
                outputPath = Paths.get(outputFile);
                Path parent = outputPath.getParent();
                if (parent != null) {
                    Files.createDirectories(parent);
                }
            } catch (InvalidPathException | IOException e) {
                throw new FileWalkerException("Failed to create parent directories '" + outputFile + "'", e);
            }

            try (BufferedReader inputFileReader = Files.newBufferedReader(Paths.get(inputFile));
                BufferedWriter outputFileWriter = Files.newBufferedWriter(outputPath)) {
                FileWalker fileWalker = new FileWalker(outputFileWriter);
                while (true) {
                    String pathName;
                    try {
                        pathName = inputFileReader.readLine();
                    } catch (IOException e) {
                        throw new FileWalkerException("Reading input file failed '" + inputFile + "'", e);
                    }

                    if (pathName == null) {
                        break;
                    }

                    try {
                        try {
                            Files.walkFileTree(Paths.get(pathName), fileWalker);
                        } catch (InvalidPathException e) {
                            fileWalker.printHash(pathName, FileWalker.HASH_ON_ERROR);
                        }
                    } catch (IOException e) {
                        throw new FileWalkerException("Failed to write to output file '" + outputFile + "'", e);
                    }
                }
            } catch (InvalidPathException | IOException e) {
                throw new FileWalkerException("Failed to open input file '" + inputFile + "'", e);
            }
        } catch (FileWalkerException e) {
            System.err.println("FileWalkerError: " + e.getMessage());
        }
    }
}