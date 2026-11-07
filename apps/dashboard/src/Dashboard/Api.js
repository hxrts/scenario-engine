// The fetch device of the API client: sends a request and resolves to its reply, its status and body
// text. A non-2xx reply resolves too; `Handlers.Api.Client` reads its error. Only a request that fails
// to reach the server rejects.
export const fetchImpl = (method) => (url) => (body) => () =>
  fetch(url, method === "GET" ? {} : { method, body, headers: { "content-type": "application/json" } })
    .then(async (r) => ({ status: r.status, body: await r.text() }));
