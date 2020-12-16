package ru.ifmo.rain.tarasov.crawler;

import info.kgeorgiy.java.advanced.crawler.CachingDownloader;
import info.kgeorgiy.java.advanced.crawler.Crawler;
import info.kgeorgiy.java.advanced.crawler.Document;
import info.kgeorgiy.java.advanced.crawler.Downloader;
import info.kgeorgiy.java.advanced.crawler.Result;
import info.kgeorgiy.java.advanced.crawler.URLUtils;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.concurrent.*;

public class WebCrawler implements Crawler {
    private final Downloader downloader;
    private final int perHost;

    private final ExecutorService downloadingService;
    private final ExecutorService extractingService;

    private final Map<String, HostStorage> processedHosts;

    private class DataContainer {
        final Set<String> traversedUrls;
        final Set<String> successive;
        final Map<String, IOException> erroneous;
        final Phaser phaser;

        DataContainer() {
            traversedUrls = ConcurrentHashMap.newKeySet();
            successive = ConcurrentHashMap.newKeySet();
            erroneous = new ConcurrentHashMap<>();
            phaser = new Phaser();
        }
    }

    /**
     * Main method to call WebCrawler via CLI
     * @param args an array of String arguments
     */
    public static void main(final String[] args) {
        Objects.requireNonNull(args);
        Objects.requireNonNull(args[0]);
        if (args.length > 5) {
            System.out.println("Usage: WebCrawler url [depth [downloads [extractors [perHost]]]]");
        }

        try {
            final int depth       = getSingleArgByNumber(args, 0, 1);
            final int downloaders = getSingleArgByNumber(args, 1, 1);
            final int extractors  = getSingleArgByNumber(args, 2, 1);
            final int perHost     = getSingleArgByNumber(args, 3, 1);

            final CachingDownloader downloader = new CachingDownloader();
            try (final WebCrawler crawler = new WebCrawler(downloader, downloaders, extractors, perHost)) {
                crawler.download(args[0], depth);
            }
        } catch (final IOException e) {
            System.out.println("Failed to create an instance of downloader, aborting...");
        } catch (final IllegalFormatException e) {
            System.out.println("Invalid integer format, check your input");
        }
    }

    private static Integer getSingleArgByNumber(final String[] args, final int number, final int defaultValue) {
        if (args.length > number && args[number] != null) {
            return Integer.parseInt(args[number]);
        } else {
            return defaultValue;
        }
    }

    /**
     * WebCrawler constructor
     * @param downloader provided {@link Downloader}
     * @param downloaders limitation for threads doing downloading
     * @param extractors limitation for threads doing extraction
     * @param perHost limitation for threads doing downloading per single Host
     */
    public WebCrawler(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
        this.downloader = downloader;
        this.perHost = perHost;

        this.downloadingService = Executors.newFixedThreadPool(downloaders);
        this.extractingService = Executors.newFixedThreadPool(extractors);

        this.processedHosts = new ConcurrentHashMap<>();
    }

    /**
     * Method that performs recursive traversal of given links.
     * @param url a URL of the start node in traversal graph.
     * @param depth download depth.
     * @return {@link Result} with successive and erroneous (mapped link-exception) sites
     */
    @Override
    public Result download(final String url, final int depth) {
        final DataContainer container = new DataContainer();

        container.traversedUrls.add(url);

        container.phaser.register();
        submitDownloadRequest(url, container, depth);
        container.phaser.arriveAndAwaitAdvance();

        return new Result(new ArrayList<>(container.successive), container.erroneous);
    }

    private void submitDownloadRequest(final String link, final DataContainer container,
                                       final int depth) {
        final String hostname;
        try {
            hostname = URLUtils.getHost(link);
        } catch (final MalformedURLException e) {
            container.erroneous.put(link, e);
            return;
        }

        container.phaser.register(); // for extractor task

        final HostStorage currentHost = processedHosts
                .computeIfAbsent(hostname, _hostname -> new HostStorage());

        currentHost.addTask(() -> {
            try {
                final Document document = downloader.download(link);
                container.successive.add(link);
                submitMultipleExtractorsTasks(depth, document, container, link);
            } catch (final IOException e) {
                container.erroneous.put(link, e);
            } finally {
                container.phaser.arrive();
            }
        });
    }

    private void submitMultipleExtractorsTasks(final int depth, final Document document,
                                               final DataContainer container, final String link) {
        if (depth - 1 > 0) {
            container.phaser.register();
            extractingService.submit(() -> {
                try {
                    document.extractLinks().forEach(newLink -> {
                        if (container.traversedUrls.add(newLink)) {
                            submitDownloadRequest(newLink, container, depth - 1);
                        }
                    });
                } catch (final IOException e) {
                    container.erroneous.put(link, e);
                } finally {
                    container.phaser.arrive();
                }
            });
        }
    }

    /**
     * Method that performs threads shutdown.
     */
    @Override
    public void close() {
        downloadingService.shutdown();
        extractingService.shutdown();
        try {
            downloadingService.awaitTermination(60, TimeUnit.SECONDS);
            extractingService.awaitTermination(60, TimeUnit.SECONDS);
        } catch (final InterruptedException e) {
            // ignore
        }
    }

    private class HostStorage {
        private int activeDownloads;
        private final Queue<Runnable> tasksForHost = new ArrayDeque<>();

        public synchronized void addTask(final Runnable task) {
            if (activeDownloads + 1 <= perHost) {
                submitWrapper(task);
                activeDownloads++;
            } else {
                tasksForHost.add(task);
            }
        }

        public synchronized void onTaskCompletion() {
            activeDownloads--;
            if (tasksForHost.isEmpty()) {
                return;
            }

            submitWrapper(tasksForHost.poll());

            activeDownloads++;
        }

        private void submitWrapper(final Runnable task) {
            downloadingService.submit(() -> {
                task.run();
                onTaskCompletion();
            });
        }
    }
}