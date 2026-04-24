import { describe, expect, it, vi } from 'vitest';
import { tlangRename } from '../src/index.js';

describe('rename hook', () => {
  it('applies provider edits through CodeMirror dispatch', async () => {
    const view = {
      state: {
        selection: {
          main: {
            head: 4,
          },
        },
      },
      dispatch: vi.fn(),
    };

    const renamed = await tlangRename(view, {
      prepareRename: vi.fn().mockResolvedValue({
        from: 3,
        to: 5,
        placeholder: 'id',
      }),
      rename: vi.fn().mockResolvedValue([
        { from: 3, to: 5, insert: 'name' },
        { from: 10, to: 12, insert: 'name' },
      ]),
      requestName: vi.fn().mockResolvedValue('name'),
    });

    expect(renamed).toBe(true);
    expect(view.dispatch).toHaveBeenCalledWith({
      changes: [
        { from: 3, to: 5, insert: 'name' },
        { from: 10, to: 12, insert: 'name' },
      ],
    });
  });

  it('returns false when prepareRename rejects the position', async () => {
    const view = {
      state: {
        selection: {
          main: {
            head: 0,
          },
        },
      },
      dispatch: vi.fn(),
    };

    const renamed = await tlangRename(view, {
      prepareRename: vi.fn().mockResolvedValue(null),
      rename: vi.fn(),
      requestName: vi.fn(),
    });

    expect(renamed).toBe(false);
    expect(view.dispatch).not.toHaveBeenCalled();
  });
});
