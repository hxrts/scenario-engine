// Selectors shared by the dashboard tests. Each names a region or a row by what the user sees, so a test
// reads the page the way a person does.

// The section headed `title`, within `scope` (the page, or a locator of the page).
export const section = (scope, title) => sectionHas(scope, scope.getByRole("heading", { name: title, exact: true }));

// The section holding `has`, a locator (for a panel with no heading to name it by).
export const sectionHas = (scope, has) => scope.locator("section").filter({ has });

// The choice list's checkbox for one run: the label whose run-id text is exactly that id.
export const choice = (page, id) => page.locator("label").filter({ has: page.getByText(id, { exact: true }) }).getByRole("checkbox");

// The row of the visible table whose cells include every given text exactly, within `scope` (the page, or a locator
// of it). Each cell is matched relative to its row, so the scope is not searched for again inside the row.
export const rowWith = (scope, cells) => {
  const page = typeof scope.page === "function" ? scope.page() : scope;
  return cells.reduce((rows, text) => rows.filter({ has: page.getByRole("cell", { name: text, exact: true }) }), scope.getByRole("row"));
};

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
