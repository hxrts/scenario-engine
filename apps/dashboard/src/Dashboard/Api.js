// Fetch a URL and resolve to its body. A non-2xx response rejects with the server's message:
// the `error` field of its JSON reply when it has one, otherwise the raw text. An error reply
// is never decoded as data.
const errorMessage = (text, r) => {
  try {
    const body = JSON.parse(text);
    if (body && typeof body.error === "string") return body.error;
  } catch (_) {
    // not JSON: use the raw text below
  }
  return text || `${r.status} ${r.statusText}`;
};

export const fetchTextImpl = (method) => (url) => (body) => () =>
  fetch(url, method === "GET" ? {} : { method, body, headers: { "content-type": "application/json" } })
    .then(async (r) => {
      const text = await r.text();
      if (!r.ok) throw new Error(errorMessage(text, r));
      return text;
    });
