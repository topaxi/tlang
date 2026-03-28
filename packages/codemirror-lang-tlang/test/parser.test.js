import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, it } from 'vitest';
import { fileTests } from '@lezer/generator/dist/test';
import { parser } from '../src/parser.js';

const __dirname = dirname(fileURLToPath(import.meta.url));

function runCorpus(fileName) {
  const content = readFileSync(join(__dirname, fileName), 'utf8');
  for (const { name, run } of fileTests(content, fileName)) {
    it(name, () => run(parser));
  }
}

describe('tagged strings', () => {
  runCorpus('tagged-strings.txt');
});

describe('triple-quoted strings', () => {
  runCorpus('triple-strings.txt');
});
