import { defineConfig } from 'vite'
import wasm from 'vite-plugin-wasm'

export default defineConfig({
  plugins: [
    wasm(),
  ],
  server: {
    fs: {
      allow: ['..']
    }
  },
  build: {
    target: 'esnext',
    rollupOptions: {
      output: {
        manualChunks: {
          "codemirror": ["codemirror", '@codemirror/state', '@codemirror/lint', 'codemirror-theme-catppuccin'],
          "codemirror-javascript": ["@codemirror/lang-javascript"],
          "codemirror-tlang": ["codemirror-lang-tlang"],
        }
      }
    }
  }
});
