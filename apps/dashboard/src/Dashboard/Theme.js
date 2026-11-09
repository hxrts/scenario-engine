// Theme preference lives in localStorage under "theme"; dark is the default.
export const readStoredTheme = () =>
  localStorage.getItem("theme") === "light" ? "light" : "dark";

export const storeTheme = (theme) => () => {
  localStorage.setItem("theme", theme);
  document.documentElement.classList.toggle("dark", theme === "dark");
};

// The public page the server marked this document as ("" for the app).
export const pageMarker = () => document.querySelector('meta[name="scenario-page"]')?.content ?? "";
