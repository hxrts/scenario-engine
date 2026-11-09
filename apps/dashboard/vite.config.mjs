import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { API_PORT } from "./ports.mjs";

const here = path.dirname(fileURLToPath(import.meta.url));

export default defineConfig({
  root: here,
  base: "/admin/",
  plugins: [react(), tailwindcss()],
  resolve: { alias: { "@": here, "@output": path.resolve(here, "../../output") } },
  server: { proxy: { "/api": `http://localhost:${API_PORT}` } },
  build: {
    outDir: "dist",
    emptyOutDir: true,
    // recharts is one ~500 KB chunk, the only large one. The real fix is to lazy-load the charts
    // on the pages that show them (see the dashboard page split); until then the limit is raised
    // to the size of that library rather than hiding a growing bundle.
    chunkSizeWarningLimit: 600,
    // Split vendor code so the app bundle stays small and the heavy libraries cache separately.
    rollupOptions: {
      output: {
        manualChunks(id) {
          if (!id.includes("node_modules")) return undefined;
          if (/node_modules\/(react|react-dom|scheduler)\//.test(id)) return "react";
          if (/node_modules\/(recharts|d3-|victory-vendor|recharts-scale|decimal\.js-light|eventemitter3|tiny-invariant)/.test(id)) return "charts";
          return "vendor";
        },
      },
    },
  },
});
