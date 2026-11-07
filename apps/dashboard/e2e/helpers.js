// Selectors shared by the dashboard tests. Each names a region or a row by what the user sees, so a test
// reads the page the way a person does.

// The section headed `title`, within `scope` (the page, or a locator of the page).
export const section = (scope, title) => scope.locator("section").filter({ has: scope.getByRole("heading", { name: title, exact: true }) });

// The row of the visible table whose cells include every given text exactly.
export const rowWith = (scope, cells) => cells.reduce((rows, text) => rows.filter({ has: scope.getByRole("cell", { name: text, exact: true }) }), scope.getByRole("row"));

// A spy on the POSTs to `url` (the run list by default): `count` is how many were sent, `body` the last one's JSON.
// The requests still go through to the server.
export function postSpy(page, url = "**/api/runs") {
  const spy = { count: 0, body: null };
  page.route(url, (route) => {
    if (route.request().method() === "POST") {
      spy.count += 1;
      spy.body = route.request().postDataJSON();
    }
    return route.continue();
  });
  return spy;
}
