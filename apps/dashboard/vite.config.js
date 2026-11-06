import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";
import path from "node:path";

export default defineConfig({
  root: __dirname,
  plugins: [react(), tailwindcss()],
  resolve: { alias: { "@": __dirname, "@output": path.resolve(__dirname, "../../output") } },
  server: { proxy: { "/api": "http://localhost:8787" } },
  build: { outDir: "dist", emptyOutDir: true },
});
