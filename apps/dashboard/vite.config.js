import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";
import path from "node:path";
import { API_PORT } from "./ports.js";

export default defineConfig({
  root: __dirname,
  plugins: [react(), tailwindcss()],
  resolve: { alias: { "@": __dirname, "@output": path.resolve(__dirname, "../../output") } },
  server: { proxy: { "/api": `http://localhost:${API_PORT}` } },
  build: { outDir: "dist", emptyOutDir: true },
});
