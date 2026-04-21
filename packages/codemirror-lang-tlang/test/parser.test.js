import { readFileSync } from 'node:fs';
import { join, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
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

describe('keyword suffix expressions', () => {
  runCorpus('keyword-suffix-operators.txt');
});

describe('type declaration names', () => {
  for (const source of ['enum Foo {}', 'struct Foo {}', 'protocol Foo {}']) {
    it(`accepts uppercase type names in "${source}"`, () => {
      const tree = parser.parse(source).toString();
      expect(tree).toContain('TypeName');
      expect(tree).not.toContain('⚠');
    });
  }

  for (const source of ['enum foo {}', 'struct foo {}', 'protocol foo {}']) {
    it(`rejects lowercase type names in "${source}"`, () => {
      expect(parser.parse(source).toString()).toContain('⚠');
    });
  }
});
