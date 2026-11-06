export const fetchTextImpl = (method) => (url) => (body) => () =>
  fetch(url, method === "GET" ? {} : { method, body, headers: { "content-type": "application/json" } })
    .then((r) => r.text());
