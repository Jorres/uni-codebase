package ru.ifmo.rain.tarasov.bank;

import org.junit.jupiter.api.*;
import org.junit.platform.launcher.Launcher;
import org.junit.platform.launcher.LauncherDiscoveryRequest;
import org.junit.platform.launcher.TestPlan;
import org.junit.platform.launcher.core.LauncherDiscoveryRequestBuilder;
import org.junit.platform.launcher.core.LauncherFactory;
import org.junit.platform.launcher.listeners.SummaryGeneratingListener;
import org.junit.platform.launcher.listeners.TestExecutionSummary;

import java.io.PrintWriter;
import java.rmi.Naming;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static org.junit.platform.engine.discovery.ClassNameFilter.includeClassNamePatterns;
import static org.junit.platform.engine.discovery.DiscoverySelectors.selectClass;

@DisplayName("Bank Tests")
public class BankTests {
    private static final int REGISTRY_PORT = 8889;
    private static final int SERVER_PORT = 8890;
    private static final int MS_TIMEOUT_TO_END_TEST = 2000;

    private static Server server;
    private static Bank bank;
    private static RequestContainer request;

    /**
     * Runner of tests via Junit5 framework.
     */
    public static class RunJUnit5TestsFromJava {
        SummaryGeneratingListener listener = new SummaryGeneratingListener();

        public void runAll() {
            LauncherDiscoveryRequest request = LauncherDiscoveryRequestBuilder.request()
                    .selectors(selectClass(BankTests.class))
                    .filters(includeClassNamePatterns(".*"))
                    .build();
            Launcher launcher = LauncherFactory.create();
            TestPlan testPlan = launcher.discover(request);
            launcher.registerTestExecutionListeners(listener);
            launcher.execute(request);
        }
    }

    public BankTests() {

    }

    /**
     * Main class for running tests via launch of the class.
     * @param args unused
     */
    public static void main(String[] args) {
        RunJUnit5TestsFromJava runner = new RunJUnit5TestsFromJava();
        runner.runAll();

        TestExecutionSummary summary = runner.listener.getSummary();
        summary.printTo(new PrintWriter(System.out));
        int result = summary.getTestsFailedCount() == 0 ? 0 : 1;
        System.exit(result);
    }

    @BeforeAll
    static void beforeAll() {
        try {
            server = new Server(SERVER_PORT, REGISTRY_PORT);
            LocateRegistry.getRegistry();

            request = new RequestContainer("Egor", "Tarasov", "0000", "1", 100);
        } catch (RemoteException e) {
            Assertions.fail("Failed to instantiate a registry, aborting...");
        }
    }

    @BeforeEach
    void beforeEach() {
        Assertions.assertDoesNotThrow(() -> {
            server.rebindBank();
            bank = (Bank) Naming.lookup(Utilities.getRegistryAddress(REGISTRY_PORT, "bank"));
        });
    }

    @AfterEach
    void afterEach() {}

    @AfterAll
    static void afterAll() {}

    @Test
    @DisplayName("Create a person with no accounts")
    void createSinglePerson() {
        Assertions.assertDoesNotThrow(() -> {
            Assertions.assertNotNull(
                    bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId));
        });
    }

    @Test
    @DisplayName("Create a person with no accounts in parallel")
    void createSinglePersonInManyThreads() {
        Assertions.assertDoesNotThrow(() -> {
            Assertions.assertNotNull(bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId));

            final int times = 10;
            final int threads = 10;

            ExecutorService service = Executors.newFixedThreadPool(threads);
            for (int i = 0; i < threads; i++) {
                service.submit(() -> {
                    for (int j = 0; j < times; j++) {
                        try {
                            Assertions.assertNotNull(bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId));
                        } catch (RemoteException ignored) {

                        }
                    }
                });
            }

            Assertions.assertEquals(1, bank.getPeopleContainerSize());
        });
    }

    @Test
    @DisplayName("Request +100 on existing person")
    void firstRequestExistingPerson() {
        Assertions.assertDoesNotThrow(() -> {
            Assertions.assertNotNull(bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId));

            Client.executeRemoteRequest(request, bank);
            AssertMoneyEqual(request.change, /* remote */ true);
        });
    }

    @Test
    @DisplayName("Request +100 on non-existing person")
    void firstRequestEmptyPerson() {
        makeRequestsOnSinglePerson(1);
    }

    @Test
    @DisplayName("Multiple requests on single person")
    void multipleRequestsOnSinglePerson() {
        makeRequestsOnSinglePerson(10);
    }

    @Test
    @DisplayName("A lot of requests on single person")
    void hugeAmountOfRequestsOnSinglePerson() {
        makeRequestsOnSinglePerson(1000);
    }

    @Test
    @DisplayName("Multithreaded requests on single person")
    void multithreadedRequestsOnSinglePerson() {
        Assertions.assertDoesNotThrow(() -> {
            Assertions.assertNotNull(bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId));

            final int times = 10;
            final int threads = 10;

            ExecutorService service = Executors.newFixedThreadPool(threads);
            for (int i = 0; i < threads; i++) {
                final RequestContainer data = request;
                service.submit(() -> {
                    for (int j = 0; j < times; j++) {
                        Client.executeRemoteRequest(data, bank);
                    }
                });
            }

            service.awaitTermination(MS_TIMEOUT_TO_END_TEST, TimeUnit.MILLISECONDS);
            AssertMoneyEqual(request.change * times * threads, /* remote */ true);
        });
    }

    @Test
    @DisplayName("Multithreaded requests on multiple people")
    void multithreadedRequestsOnMultiplePeople() {
        Assertions.assertDoesNotThrow(() -> {
            final List<String> names = Arrays.asList("Amir", "Betty", "Cindy", "Egor");
            final List<String> surnames = Arrays.asList("A", "B", "C", "E");
            final List<String> ids = Arrays.asList("0000", "0001", "0002", "0003");

            Assertions.assertNotNull(bank.getOrDefaultRemotePerson("Amir", "A", "0000"));
            Assertions.assertNotNull(bank.getOrDefaultRemotePerson("Betty", "B", "0001"));
            Assertions.assertNotNull(bank.getOrDefaultRemotePerson("Cindy", "C", "0002"));
            Assertions.assertNotNull(bank.getOrDefaultRemotePerson("Egor", "E", "0003"));

            final int money = 100;
            final int times = 10;
            final int threads = 10;
            final int sampleLength = 4;

            ExecutorService service = Executors.newFixedThreadPool(threads);
            for (int i = 0; i < threads; i++) {
                service.submit(() -> {
                    for (int j = 0; j < sampleLength * times; j++) {
                        RequestContainer container = new RequestContainer(names.get(j % sampleLength),
                                surnames.get(j % sampleLength),
                                ids.get(j % sampleLength),
                                "1", money);
                        Client.executeRemoteRequest(container, bank);
                    }
                });
            }

            service.awaitTermination(MS_TIMEOUT_TO_END_TEST, TimeUnit.MILLISECONDS);

            Assertions.assertEquals(money * threads * times,
                    bank.getOrDefaultRemotePerson("Amir", "A", "0000").getAmount("1"));
            Assertions.assertEquals(money * threads * times,
                    bank.getOrDefaultRemotePerson("Betty", "B", "0001").getAmount("1"));
            Assertions.assertEquals(money * threads * times,
                    bank.getOrDefaultRemotePerson("Cindy", "C", "0002").getAmount("1"));
            Assertions.assertEquals(money * threads * times,
                    bank.getOrDefaultRemotePerson("Egor", "E", "0003").getAmount("1"));
        });
    }

    @Test
    @DisplayName("Attempt to modify a local person")
    void fetchLocalPerson() {
        Assertions.assertDoesNotThrow(() -> {
            Assertions.assertNotNull(bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId));

            Client.executeLocalRequest(request, bank);

            AssertMoneyEqual(0, /* remote */ true);
            AssertMoneyEqual(0, /* remote */ false);
        });
    }

    @Test
    @DisplayName("Change on remote person reflects on each")
    void checkRemoteReflection() {
        Assertions.assertDoesNotThrow(()    -> {
            Assertions.assertNotNull(bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId));

            final int copies = 10;
            ArrayList<IRemotePerson> iRemoteCopies = new ArrayList<>();
            for (int i = 0; i < copies; i++) {
                iRemoteCopies.add(bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId));
            }

            Client.executeRemoteRequest(request, bank);

            for (int i = 0; i < copies; i++) {
                Assertions.assertEquals(request.change, // change should reflect on every remote copy
                        iRemoteCopies.get(i).getAmount(request.accountId));
            }
        });
    }

    @Test
    @DisplayName("Change on local person is not reflected")
    void checkLocalReflection() {
        Assertions.assertDoesNotThrow(() -> {

            Assertions.assertNotNull(bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId));

            final int copies = 10;
            ArrayList<ILocalPerson> iLocalCopies = new ArrayList<>();
            for (int i = 0; i < copies; i++) {
                iLocalCopies.add(bank.getOrDefaultLocalPerson(request.name, request.surname, request.passportId));
            }

            Client.executeRemoteRequest(request, bank);

            for (int i = 0; i < copies; i++) {
                Assertions.assertEquals(0, // should not reflect on snapshots
                        iLocalCopies.get(i).getAmount(request.accountId));
            }
        });
    }

    private static void makeRequestsOnSinglePerson(final int times) {
        Assertions.assertDoesNotThrow(() -> {
            Assertions.assertNotNull(bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId));

            for (int i = 0; i < times; i++) {
                Client.executeRemoteRequest(request, bank);
            }
            AssertMoneyEqual(request.change * times, /* remote */ true);
        });
    }

    private static void AssertMoneyEqual(final int expected, final boolean isRemote) {
        Assertions.assertDoesNotThrow(() -> {
            int result;
            if (isRemote) {
                result = bank.getOrDefaultRemotePerson(request.name, request.surname, request.passportId)
                        .getAmount(request.accountId);
            } else {
                result = bank.getOrDefaultLocalPerson(request.name, request.surname, request.passportId)
                        .getAmount(request.accountId);
            }
            Assertions.assertEquals(expected, result);
        });
    }
}
