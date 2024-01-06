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
  }
});
