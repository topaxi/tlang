import { defineConfig } from 'vite';
import wasm from 'vite-plugin-wasm';

export default defineConfig({
  plugins: [wasm()],
  server: {
    fs: {
      allow: ['..'],
    },
  },
  build: {
    target: 'esnext',
    sourcemap: true,
    rollupOptions: {
      output: {
        manualChunks(id, _meta) {
          let codemirrorShared = [
            'node_modules/codemirror',
            'node_modules/@codemirror/state',
            'node_modules/@codemirror/lint',
            'node_modules/codemirror-theme-catppuccin',
          ];

          if (id.endsWith('.tlang?raw')) {
            return 'examples';
          }

          if (codemirrorShared.some((path) => id.includes(path))) {
            return 'codemirror';
          }

          if (id.includes('@codemirror/lang-javascript')) {
            return 'codemirror-javascript';
          }

          if (id.includes('codemirror-lang-tlang')) {
            return 'codemirror-tlang';
          }
        },
      },
    },
  },
});
