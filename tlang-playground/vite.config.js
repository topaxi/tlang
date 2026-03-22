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
          if (id.includes('examples') && id.endsWith('.tlang?raw')) {
            return 'examples';
          }

          if (id.includes('tlang_bindings')) {
            return 'tlang_bindings_js';
          }

          let vendor_ui = ['@lit', 'lit-html', '@floating'];
          if (vendor_ui.some((path) => id.includes(path))) {
            return 'vendor-ui';
          }

          let cmt;
          if (
            (cmt =
              /@codemirror\/theme-(?<theme>\w+)|codemirror-theme-(?<theme>\w+)/.exec(
                id,
              ))
          ) {
            return `codemirror/theme/${cmt.groups.theme}`;
          }

          let cml;
          if (
            (cml =
              /@codemirror\/lang-(?<lang>\w+)|codemirror-lang-(?<lang>\w+)/.exec(
                id,
              ))
          ) {
            return `codemirror/lang/${cml.groups.lang}`;
          }

          let codemirrorShared = [
            'node_modules/codemirror',
            'node_modules/@codemirror/state',
            'node_modules/@codemirror/lint',
          ];
          if (codemirrorShared.some((path) => id.includes(path))) {
            return 'codemirror';
          }
        },
      },
    },
  },
});
