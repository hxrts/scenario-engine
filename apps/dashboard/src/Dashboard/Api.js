// Fetch a URL and resolve to its body. A non-2xx response rejects with the server's message,
// so an error reply is never decoded as data.
export const fetchTextImpl = (method) => (url) => (body) => () =>
  fetch(url, method === "GET" ? {} : { method, body, headers: { "content-type": "application/json" } })
    .then(async (r) => {
      const text = await r.text();
      if (!r.ok) throw new Error(text || `${r.status} ${r.statusText}`);
      return text;
    });
