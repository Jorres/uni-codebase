package ru.ifmo.rain.tarasov.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.*;
import java.io.*;
import java.lang.reflect.*;
import java.net.URISyntaxException;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.jar.*;
import java.util.zip.ZipEntry;

/**
 * Implementation class for {@link JarImpler} interface
 */
public class Implementor implements JarImpler {
    /**
     * Main is used to choose jar-mode or simple mode of execution.
     * Executes {@link JarImplementor} in two ways: <br>
     *  2 arguments: {@code className rootPath} - executes {@link #implement(Class, Path)} with arguments 0, 1<br>
     *  3 arguments: {@code -jar className jarPath} - executes {@link #implementJar(Class, Path)} with arguments 1, 2<br>
     *
     * @param args mode, if present, and remaining: filename and target filename
     */
    public static void main(final String[] args) {
        Objects.requireNonNull(args);
        try {
            if ((args.length == 3 && args[0].equals("-jar")) || (args.length == 2)) {
                Class<?> token = null;
                final int filename = (args[0].equals("-jar") ? 1 : 0);
                final int filepath = filename + 1;

                try {
                    token = Class.forName(args[filename], false, JarImplementor.class.getClassLoader());
                } catch (final ClassNotFoundException e) {
                    throwCompleteException("Unable to load given class $" + e.getMessage(), e);
                }

                if (args.length == 3) {
                    new Implementor().implementJar(token, Paths.get(args[filepath]));
                } else {
                    new Implementor().implement(token, Paths.get(args[filepath]));
                }
            } else {
                throw new ImplerException("Usage: -jar class_name filename.jar");
            }
        } catch (final ImplerException e) {
            e.printStackTrace(System.out);
        }
    }

    /**
     * Combines path, token name and extension into a single {@link Path} instance.
     * @param path parent from a given filename
     * @param token instance of type-token for generated class
     * @param extension filename extension
     * @return {@link Path} representing path to certain file
     */
    private static Path getFilePath(final Path path, final Class<?> token, final String extension) {
        return path.resolve(token.getPackageName().replace('.', File.separatorChar))
                .resolve(token.getSimpleName() + "Impl" + extension);
    }

    /**
     * Produces {@code .jar} file with implementation requested by {@code token}.
     *
     * Generated class full name is token filename +  {@code Impl} suffix
     * added. <br>
     *
     * Also creates temporary directory to store temporary {@code .java} and {@code .class} files.
     * Throws an exception if failed to delete those. <br>
     * @throws ImplerException if the given class cannot be generated some of the following reasons:
     *
     *  Some arguments are {@code null}, or <br>
     *  Error occurs during implementation with {@link #implement(Class, Path)}, or <br>
     *  {@link JavaCompiler} failed to compile implemented class, or <br>
     *  The problems with I/O occurred during implementation. <br>
     *
     */
    @Override
    public void implementJar(final Class<?> token, final Path jarFile) throws ImplerException {
        final Path tempDir;
        try {
            tempDir = Files.createTempDirectory(jarFile.toAbsolutePath().getParent(), "temp");
        } catch (final IOException e) {
            throw new ImplerException("Tmp directory for build files not created", e);
        }

        try {
            implement(token, tempDir);

            final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
            final String finalJavaFilePath = getFilePath(tempDir, token, ".java").toString();
            Path packagePath = Path.of(token.getPackageName().replace('.', File.separatorChar));
            final String[] args = new String[] {
                    "-cp",
                    tempDir.toString() + File.pathSeparator + getClassPath(token),
                    "-encoding",
                    "utf8",
                    finalJavaFilePath
            };

            if (compiler == null || compiler.run(null, null, null, args) != 0) {
                throw new ImplerException("Generated files were not compiled");
            }

            final Manifest manifest = new Manifest();
            final Attributes attributes = manifest.getMainAttributes();
            attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");

            try (final JarOutputStream writer = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
                writer.putNextEntry(new ZipEntry(packagePath.resolve(token.getSimpleName() + "Impl.class").toString()
                    .replace('\\', '/')));
                Files.copy(tempDir.resolve(packagePath).resolve(token.getSimpleName() + "Impl.class"), writer);
            } catch (final IOException e) {
                throw new ImplerException("Unable to write to JAR file", e);
            }
        } finally {
            try {
                clean(tempDir);
            } catch (final IOException e) {
                System.out.println("Unable to delete temp directory: " + e.getMessage());
            }
        }
    }

    /**
     * Helper function to retrieve all {@code .class} files and give the compiler a proper classpath.
     * @param token token that provides classpath
     * @return {@link String} representation of the classpath
     * @throws ImplerException if unable to locate resource via the URI
     */
    private String getClassPath(final Class<?> token) throws ImplerException {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new ImplerException("Unable to get code source " + e.getMessage(), e);
        }
    }

    /**
     * Performs the cleanup of the temporary folder.
     * @param path represents the folder to recursively delete.
     * @throws IOException In case {@link SimpleFileVisitor} fails to delete some of the files
     */
    private static void clean(final Path path) throws IOException {
        Files.walkFileTree(path, CLEANER);
    }
    /**
     * Singleton of instance {@link TempFolderCleaner} for performing temporary directory cleanup
     */
    private static final TempFolderCleaner CLEANER = new TempFolderCleaner();

    /**
     * Used for recursively deleting the temporary folder with temporary implementation files
     */
    private static class TempFolderCleaner extends SimpleFileVisitor<Path> {
        /**
         * Creates new instance of TempFolderCleaner
         */
        TempFolderCleaner(){
            super();
        }

        /**
         * Deletes file described by {@code file} {@link Path} instance
         * @param file path, describing current visited file
         * @param attrs file attributes
         * @return {@link FileVisitResult#CONTINUE}
         * @throws IOException if error occured during deletion process
         */
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        /**
         * Deletes file described by {@link Path} instance dir.
         * @param dir path, describing current visited file
         * @param exception IOException stored that caused traversal to quit prematurely, or null if
         *            iteration completed succesfully.
         * @return {@link FileVisitResult#CONTINUE}
         * @throws IOException if error occured during deletion process
         */
        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exception) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    }

    /**
     * Actual logic for code generation starts here.
     * @param token type token to create implementation for.
     * @param root root directory.
     * @throws ImplerException if code generation failed for some of the reasons <br>
     *    Some arguments are {@code null} <br>
     *    Given {@code class} is primitive or array. <br>
     *    {@code class} isn't an interface and contains only private constructors,
     *      in this case instantiation is impossible <br>
     *    IOException was thrown somewhere along the generation process
     */
    @Override
    public void implement(final Class<?> token, final Path root) throws ImplerException {
        Objects.requireNonNull(token);
        Objects.requireNonNull(root);

        if (token.isPrimitive() || token.isArray() || token == Enum.class
                || Modifier.isFinal(token.getModifiers())
                || Modifier.isPrivate(token.getModifiers())) {
            throw new ImplerException("Unable to generate implementation for this type.");
        }

        if (!token.isInterface() && Arrays.stream(token.getDeclaredConstructors())
                .allMatch(constructor -> Modifier.isPrivate(constructor.getModifiers()))) {
            throw new ImplerException("All constructors in given class are private.");
        }

        final Path filePath;
        try {
            filePath = Paths.get(root.toString(),
                    token.getPackageName().replace('.', File.separatorChar),
                    token.getSimpleName() + "Impl.java");
        } catch (final InvalidPathException e) {
            throw new ImplerException(e);
        }

        final Path parent = filePath.getParent();
        if (parent != null) {
            try {
                Files.createDirectories(parent);
            } catch (final IOException e) {
                throwCompleteException("Unable to create directories.", e);
            }
        }

        try (final BufferedWriter writer = Files.newBufferedWriter(filePath)) {
            final Generator generator = new Generator(writer, token);
            generator.generate();
        } catch (final IOException e) {
            throwCompleteException("Unable to write to output file.", e);
        }
    }

    /**
     * Custom shortcut for throwing an exception, containing wrapper and exception itself.
     * @param message custom {@link String} message in wrapper
     * @param e {@link Exception} that happened in the first place
     * @throws ImplerException the resulted wrapper containing both the information about the
     * initial exception as well as custom message.
     */
    private static void throwCompleteException(final String message, final Exception e) throws ImplerException {
        throw new ImplerException(message + " " + e.getMessage(), e);
    }

    /**
     * Helper class that incapsulates the task of IO when {@link #implement(Class, Path)} is called.
     */
    private static class Generator {
        /**
         * Simple-to-use constant containing {@link System#lineSeparator()}
         */
        private static final String LINE_SEPARATOR = System.lineSeparator();
        /**
         * Standard indentation for well-formatted code.
         */
        private static final String FOUR_SPACES = "   ";
        /**
         * Double indentation for well-formatted code.
         */
        private static final String EIGHT_SPACES = FOUR_SPACES + FOUR_SPACES;
        /**
         * Simple-to-use {@link String} view of the given token, including package divided with slashes.
         */
        private static String THIS_CLASS_NAME;

        /**
         * Class implementing {@link BufferedWriter} that handles output to resulting generated file.
         */
        private final BufferedWriter writer;
        /**
         * {@link Class} instance, demanding generation
         */
        private final Class<?> token;

        /**
         * Public constructor of {@link Generator} class
         * @param writer handler for output into the generated file
         * @param token {@link Class} instance, demanding generation
         */
        public Generator(final BufferedWriter writer, final Class<?> token) {
            this.writer = writer;
            this.token  = token;
            THIS_CLASS_NAME = token.getSimpleName() + "Impl";
        }

        /**
         * Basic sequence of steps required for generation
         * @throws ImplerException if generation failed for some of the following reasons:
         *    Some arguments are {@code null} <br>
         *    Given {@code class} is primitive or array. <br>
         *    {@code class} isn't an interface and contains only private constructors,
         *      in this case instantiation is impossible <br>
         *    IOException was thrown somewhere along the generation process
         */
        public void generate() throws ImplerException {
            writePackage();
            writeLine();
            writeDeclaration();
            writeConstructors();
            writeMembers();
            writeLine("}");
        }

        /**
         * Helper function to prepend space before writing some part of code.
         * @param s the string ready to be prepended
         * @return {@link String} value with space attached, if source String was non-empty.
         */
        private String prependSpace(final String s) {
            return s.equals("") ? s : " " + s;
        }

        /**
         * Helper function designed to create arguments to include in method signature.
         * @param parameterTypes An Array of {@link Class} instances describing types of arguments
         * @return String value, consisting of pseudo-signature with arguments separated with ', '.
         */
        private String argumentsToString(final Class<?>[] parameterTypes) {
            final StringBuilder stringBuilder = new StringBuilder();
            int index = 0;
            for (final Class<?> parameterType : parameterTypes) {
                if (index > 0) {
                    stringBuilder.append(", ");
                }
                stringBuilder.append(parameterType.getCanonicalName()).append(" arg").append(index++);
            }
            return stringBuilder.toString();
        }

        /**
         * Helper function that actually handles the dirty work and combines parts
         * of constructor or function into corresponding text representation.
         * @param modifiers modifiers to prepend before name declaration
         * @param returnType return type of method, or empty if called with constructor
         * @param name actual function name, taken from an interface this class is implementing
         * @param parameterTypes An Array of {@link Class} instances describing types of arguments
         * @param exceptionTypes string view of a set of exceptions this constructor can throw, or
         *                       empty for function (because of default implementation)
         * @param body an actual body of the function, super(...) for constructor or default
         *             {@code return default_null_value} for method
         * @throws ImplerException wrapper  for {@link IOException} that can happen on this stage
         */
        private void writeConstructorOrMethod(final String modifiers, final String returnType, final String name,
                                              final Class<?>[] parameterTypes,
                                              final String exceptionTypes, final List<String> body) throws ImplerException {
            writeLine(FOUR_SPACES, prependSpace(modifiers), prependSpace(returnType), prependSpace(name), '(',
                    argumentsToString(parameterTypes), ") ", prependSpace(exceptionTypes), "{");
            writeLine(EIGHT_SPACES, String.join("", body));
            writeLine(FOUR_SPACES, '}');
        }

        /**
         * Helper function to get methods declared in an interface
         * @param map a storage for all the abstract methods
         * @param methods an array of {@link Method} instances, belonging to one of superclasses
         *                or interfaces
         */
        private void getMethods(final Map<Signature, Boolean> map, final Method[] methods) {
            for (final Method method : methods) {
                map.putIfAbsent(new Signature(method), Modifier.isAbstract(method.getModifiers()));
            }
        }

        /**
         * Helper function to collect all the abstract members up the inheritance tree
         * and generate default implementation for them
         * @throws ImplerException {@link IOException} that can happen on this stage
         */
        private void writeMembers() throws ImplerException {
            final Map<Signature, Boolean> methods = new HashMap<>();
            getMethods(methods, token.getMethods());
            for (Class<?> clazz = token; clazz != null; clazz = clazz.getSuperclass()) {
                getMethods(methods, clazz.getDeclaredMethods());
            }

            final List<Method> result = new ArrayList<>();
            for (final Signature s : methods.keySet()) {
                if (methods.get(s)) {
                    result.add(s.getMethod());
                }
            }

            writeMethodsWithBlankLines(result);
        }

        /**
         * Helper class designed to make {@link Method} hashable and thus, insertable to {@link HashSet}
         */
        private class Signature {
            /**
             * First part of function signature, actual function name
             */
            private final String name;
            /**
             *  Second part of the method signature, argument types
             */
            private final Class<?> []args;

            /**
             * Instance of {@link Method} class currently being hashable
             */
            private final Method method;

            /**
             * Default constructor, accepts {@link Method} and dissects it
             * into name and arguments.
             * @param method method to disassemble into parts
             */
            public Signature(final Method method) {
                this.method = method;
                this.name = method.getName();
                this.args = method.getParameterTypes();
            }

            /**
             * Default getter, IDEA-generated
             * @return {@link Method} value, stored inside the Signature
             */
            public Method getMethod() {
                return method;
            }

            /**
             * IDEA-generated implementation of {@link Object#equals}
             * @param o second object to compare
             * @return true if objects are equal, false otherwise
             */
            @Override
            public boolean equals(final Object o) {
                if (this == o) return true;
                if (o == null || getClass() != o.getClass()) return false;

                final Signature signature = (Signature) o;

                if (!Arrays.equals(args, signature.args)) return false;
                return name.equals(signature.name);
            }

            /**
             * IDEA-generated implementation of {@link Object}.hashCode
             * @return the hashcode value calculated for {@link Signature} instance
             */
            @Override
            public int hashCode() {
                int result = Arrays.hashCode(args);
                result = 31 * result + name.hashCode();
                return result;
            }
        }

        /**
         * Helper function designed to write first available default constructor.
         * @throws ImplerException if write is unsuccessful
         */
        private void writeConstructors() throws ImplerException {
            for (final Constructor<?> constructor : token.getDeclaredConstructors()) {
                if (!Modifier.isPrivate(constructor.getModifiers())) {
                    writeConstructor(constructor);
                    writeLine();
                    break;
                }
            }
        }

        /**
         * Assembling of all the pieces a constructor is made of
         * @param c constructor to collect data for
         * @throws ImplerException if write is unsuccessful
         */
        private void writeConstructor(final Constructor<?> c) throws ImplerException {
            final Class<?>[] paramTypes = c.getParameterTypes();
            writeConstructorOrMethod("public", "", THIS_CLASS_NAME, paramTypes,
                    getExceptionTypes(c), List.of("super(", getArgsForConstructor(paramTypes.length), ");"));
        }

        /**
         * get pseudo-signature part looking like this: "arg1, arg2 ... argN"
         * to pass it further along inside of "super(...)" statement
         * @param n the length of sequence
         * @return the {@link String}, representing content described above
         */
        private String getArgsForConstructor(final int n) {
            final StringBuilder stringBuilder = new StringBuilder();
            for (int i = 0; i < n; i++) {
                if (i != 0) {
                    stringBuilder.append(", ");
                }
                stringBuilder.append("arg").append(i);
            }
            return stringBuilder.toString();
        }

        /**
         * Accumulator for all the exception thrown from a constructor.
         * @param c a constructor to extract exceptions from
         * @return {@link String} representation of data
         */
        private String getExceptionTypes(final Constructor<?> c) {
            final Class<?>[] exceptionTypes = c.getExceptionTypes();
            if (exceptionTypes.length == 0) {
                return "";
            }
            StringBuilder sb = new StringBuilder();
            sb.append("throws ");
            for (int i = 0; i < exceptionTypes.length - 1; i++) {
                sb.append(exceptionTypes[i].getCanonicalName());
                sb.append(", ");
            }
            sb.append(exceptionTypes[exceptionTypes.length - 1].getCanonicalName());
            return sb.toString();
        }

        /**
         * Function helper to separate all given methods with a blank line
         * @param list list with content to separate
         * @throws ImplerException if write was unsuccessful
         */
        private void writeMethodsWithBlankLines(final List<Method> list) throws ImplerException {
            boolean first = true;
            for (final Method t : list) {
                if (!first) {
                    writeLine();
                }
                first = false;
                writeMethod(t);
            }
        }

        /**
         * Assembling of all the pieces a method is made of
         * @param m a method to extract data from
         * @throws ImplerException if write was unsuccessful
         */
        private void writeMethod(final Method m) throws ImplerException {
            final Class<?> returnType = m.getReturnType();
            writeConstructorOrMethod(getAccessModifier(m), returnType.getCanonicalName(), m.getName(),
                    m.getParameterTypes(), "", List.of("return", getDefaultValue(returnType), ";"));
        }

        /**
         * Helper to get a null-associated value depending on a returnType
         * @param returnType token representing part of the function signature
         *                   to determine return type
         * @return {@link String} representation of null value
         */
        private String getDefaultValue(final Class<?> returnType) {
            if (!returnType.isPrimitive()) {
                return " null";
            }
            if (returnType.equals(void.class)) {
                return "";
            }
            if (returnType.equals(boolean.class)) {
                return " false";
            }
            return " 0";
        }

        /**
         * Helper function to get access modifier for a {@link Method}
         * @param method an argument to determine modifier for
         * @return {@link String} representation of modifier
         */
        private String getAccessModifier(final Method method) {
            final int modifiers = method.getModifiers();
            if (Modifier.isPrivate(modifiers)) {
                return "private";
            }
            if (Modifier.isProtected(modifiers)) {
                return "protected";
            }
            if (Modifier.isPublic(modifiers)) {
                return "public";
            }
            return "";
        }

        /**
         * Converts given {@link String} to unicode escaping
         * @param in string to convert
         * @return converted string
         */
        private static String toUnicode(final String in) {
            StringBuilder b = new StringBuilder();;
            for (char c : in.toCharArray()) {
                b.append(c >= 128 ? String.format("\\u%04x", (int) c) : c);
            }
            return b.toString();
        }

        /**
         * The lowest-level function, where {@link BufferedWriter#write(String)} actually happens.
         * @param args A variadic set of Objects to print
         * @throws ImplerException wrapper for IOException, if occured
         */
        private void writeLine(final Object... args) throws ImplerException {
            try {
                for (final Object arg : args) {
                    writer.write(toUnicode(arg.toString()));
                }
                writer.write(LINE_SEPARATOR);
            } catch (final IOException e) {
                throwCompleteException("Unable to write to the output file.", e);
            }
        }

        /**
         * Function helper to generate package name for class.
         * Does nothing if package is empty.
         * @throws ImplerException if write was unsuccessful.
         */
        private void writePackage() throws ImplerException {
            final StringBuilder res = new StringBuilder();
            if (!token.getPackageName().equals("")) {
                res.append("package ").append(token.getPackageName()).append(";");
                writeLine(res.toString());
            }
        }

        /**
         * Function helper to generate class declaration with interface or superclass inheritance
         * @throws ImplerException is write was unsuccessful
         */
        private void writeDeclaration() throws ImplerException {
            writeLine("public class ", THIS_CLASS_NAME, (token.isInterface() ? " implements " : " extends "),
                    token.getCanonicalName(), " {");
        }
    }
}
