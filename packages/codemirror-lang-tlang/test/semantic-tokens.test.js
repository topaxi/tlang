import { EditorState } from '@codemirror/state';
import { describe, expect, it } from 'vitest';
import { tlangLanguageSupport, tlangSemanticTokens } from '../src/index.js';

describe('semantic token hooks', () => {
  it('accepts static semantic token data', () => {
    expect(() =>
      EditorState.create({
        doc: 'fn add(x) { x }',
        extensions: [
          tlangSemanticTokens([
            {
              from: 3,
              to: 6,
              type: 'function',
              modifiers: ['declaration'],
            },
          ]),
        ],
      }),
    ).not.toThrow();
  });

  it('wires semantic token options through language support', () => {
    expect(() =>
      EditorState.create({
        doc: 'fn add(x) { x }',
        extensions: [
          tlangLanguageSupport({
            semanticTokens: [
              {
                from: 3,
                to: 6,
                type: 'function',
                modifiers: ['declaration'],
              },
            ],
          }),
        ],
      }),
    ).not.toThrow();
  });
});
