#!/usr/bin/env node
/**
 * Minimal static file server for Playwright E2E tests.
 * Serves the `dist/` directory under the `/tlang` base path so that the
 * pre-built assets (which reference `/tlang/…` paths) load correctly.
 */
import { createServer } from 'node:http';
import { readFileSync, existsSync } from 'node:fs';
import { join, extname, resolve, relative } from 'node:path';
import { fileURLToPath } from 'node:url';

const __dirname = fileURLToPath(new URL('.', import.meta.url));
const distDir = resolve(join(__dirname, '..', 'dist'));
const base = '/tlang';
const port = 4173;

/** @type {Record<string, string>} */
const mimeTypes = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'application/javascript',
  '.css': 'text/css',
  '.wasm': 'application/wasm',
  '.png': 'image/png',
  '.svg': 'image/svg+xml',
  '.json': 'application/json',
};

createServer((req, res) => {
  try {
    let pathname = (req.url ?? '/').split('?')[0];

    if (pathname.startsWith(base)) {
      pathname = pathname.slice(base.length);
    }

    if (!pathname || pathname === '/') {
      pathname = '/index.html';
    }

    const filePath = resolve(join(distDir, pathname));

    // Prevent path traversal: resolved path must stay within distDir
    const relativePath = relative(distDir, filePath);
    if (relativePath.startsWith('..') || relativePath === '' || relativePath.includes('..' + require('node:path').sep)) {
      res.writeHead(403);
      res.end('Forbidden');
      return;
    }

    if (existsSync(filePath)) {
      const data = readFileSync(filePath);
      const ext = extname(filePath);
      res.writeHead(200, {
        'Content-Type': mimeTypes[ext] ?? 'application/octet-stream',
      });
      res.end(data);
    } else {
      // SPA fallback
      const indexPath = join(distDir, 'index.html');
      if (!existsSync(indexPath)) {
        res.writeHead(500);
        res.end('index.html not found — run the build first');
        return;
      }
      const data = readFileSync(indexPath);
      res.writeHead(200, { 'Content-Type': 'text/html; charset=utf-8' });
      res.end(data);
    }
  } catch (err) {
    console.error('Request error:', err);
    res.writeHead(500);
    res.end('Internal server error');
  }
}).listen(port, () => {
  console.log(`Static server: http://localhost:${port}${base}`);
});
