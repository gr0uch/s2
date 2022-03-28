if (!globalThis.__dev_events__) {
  globalThis.__dev_events__ = new EventSource("/dev-events");
  const eventSource = globalThis.__dev_events__;

  eventSource.addEventListener("message", async (event) => {
    const changes = JSON.parse(event.data);
    const update = changes.find(({ type }) => type === "update");
    if (!update) return;
    const url = new URL(import.meta.url);
    const params = new URLSearchParams(url.searchParams);

    // Setting a unique value forces a new request.
    params.set("ts", Date.now());

    await import(`${url.origin}${url.pathname}?${params}`);
    console.log(`did replacement due to ${update.path}`);
  });

  eventSource.addEventListener("error", () => {
    eventSource.close();
  });
}
