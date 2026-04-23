import { EditorState } from '@codemirror/state';
import { afterEach, describe, expect, it, vi } from 'vitest';
import {
  __testing,
  tlangLanguageSupport,
  tlangSemanticTokens,
} from '../src/index.js';

afterEach(() => {
  vi.useRealTimers();
});

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

  it('applies only the latest async provider result', async () => {
    vi.useFakeTimers();

    let code = 'first';
    const applied = [];
    const deferred = [];
    const provider = vi.fn(() => {
      let resolve;
      const promise = new Promise((res) => {
        resolve = res;
      });
      deferred.push({ promise, resolve });
      return promise;
    });

    const scheduler = new __testing.SemanticTokenScheduler({
      provider,
      debounceMs: 75,
      getCode: () => code,
      applyTokens: (tokens) => applied.push(tokens),
    });

    scheduler.schedule();
    await vi.advanceTimersByTimeAsync(75);
    expect(provider).toHaveBeenCalledTimes(1);
    expect(provider).toHaveBeenLastCalledWith('first');

    code = 'second';
    scheduler.schedule();
    await vi.advanceTimersByTimeAsync(75);
    expect(provider).toHaveBeenCalledTimes(2);
    expect(provider).toHaveBeenLastCalledWith('second');

    deferred[1].resolve([{ from: 0, to: 6, type: 'function' }]);
    await deferred[1].promise;
    await Promise.resolve();

    deferred[0].resolve([{ from: 0, to: 5, type: 'variable' }]);
    await deferred[0].promise;
    await Promise.resolve();

    expect(applied).toEqual([[{ from: 0, to: 6, type: 'function' }]]);

    scheduler.destroy();
  });
});
