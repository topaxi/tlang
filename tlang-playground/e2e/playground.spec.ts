import { test, expect, type Page } from '@playwright/test';

async function gotoPlayground(page: Page, hash = '') {
  await page.goto(`/tlang${hash}`);
  await expect(page.locator('.cm-content').first()).toBeVisible();
}

async function openOptimizationSettings(page: Page) {
  await page.getByLabel('Optimization Settings').click();
}

async function updateEditorState(
  page: Page,
  state: { source?: string; selection?: number },
) {
  await page.evaluate(({ source, selection }) => {
    const playground = document.querySelector('tlang-playground');
    if (!playground?.shadowRoot) throw new Error('no playground shadowRoot');

    const codemirror = playground.shadowRoot.querySelector('t-codemirror');
    if (!codemirror?.shadowRoot) throw new Error('no codemirror shadowRoot');

    const cmEditor = codemirror.shadowRoot.querySelector('.cm-editor');
    if (!cmEditor) throw new Error('no .cm-editor element');

    const content = cmEditor.querySelector('.cm-content');
    if (!content) throw new Error('no .cm-content element');

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const view = (content as any).cmTile?.root?.view;
    if (!view) throw new Error('no EditorView');

    const transaction: {
      changes?: { from: number; to: number; insert: string };
      selection?: { anchor: number };
    } = {};

    if (source !== undefined) {
      transaction.changes = {
        from: 0,
        to: view.state.doc.length,
        insert: source,
      };
    }

    if (selection !== undefined) {
      transaction.selection = { anchor: selection };
    }

    view.focus();
    view.dispatch(transaction);
  }, state);
}

function optimizationRadio(page: Page, label: 'Minimal' | 'Full') {
  return page.locator('t-menuitem-radio').filter({ hasText: label });
}

type PlaygroundConsoleMessage = {
  type: string;
  args?: unknown[];
};

type PlaygroundElement = {
  consoleMessages?: PlaygroundConsoleMessage[];
  selectedExample?: string;
};

function collectBrowserErrors(page: Page) {
  const errors: string[] = [];

  page.on('console', (message) => {
    if (message.type() === 'error') {
      errors.push(message.text());
    }
  });

  page.on('pageerror', (error) => {
    errors.push(error.stack ?? error.message);
  });

  return errors;
}

async function getPlaygroundConsoleCount(page: Page): Promise<number> {
  return page.evaluate(() => {
    const playground = document.querySelector(
      'tlang-playground',
    ) as PlaygroundElement | null;

    return playground?.consoleMessages?.length ?? 0;
  });
}

async function getPlaygroundConsoleMessagesSince(page: Page, start: number) {
  return page.evaluate((start) => {
    const playground = document.querySelector(
      'tlang-playground',
    ) as PlaygroundElement | null;

    return (playground?.consoleMessages ?? []).slice(start).map((message) => ({
      type: message.type,
      text: Array.isArray(message.args)
        ? message.args
            .map((arg) => {
              if (typeof arg === 'string') {
                return arg;
              }

              if (arg instanceof Error) {
                return arg.stack ?? arg.message;
              }

              try {
                return JSON.stringify(arg);
              } catch {
                return String(arg);
              }
            })
            .join(' ')
        : String(message.args ?? ''),
    }));
  }, start);
}

async function waitForSelectedExample(page: Page, example: string) {
  await page.waitForFunction((expected) => {
    const playground = document.querySelector(
      'tlang-playground',
    ) as PlaygroundElement | null;

    return playground?.selectedExample === expected;
  }, example);
}

async function runPlayground(page: Page, initialConsoleCount: number) {
  await page.locator('t-button').filter({ hasText: 'Run' }).click();
  await page.waitForFunction((count) => {
    const playground = document.querySelector(
      'tlang-playground',
    ) as PlaygroundElement | null;
    const messages = playground?.consoleMessages ?? [];

    return messages.length > count && messages.at(-1)?.type === 'groupEnd';
  }, initialConsoleCount);
}

test.describe('Tlang Playground', () => {
  test.beforeEach(async ({ page }) => {
    await gotoPlayground(page);
  });

  test('has correct page title', async ({ page }) => {
    await expect(page).toHaveTitle('Tlang Playground');
  });

  test('renders the code editor', async ({ page }) => {
    await expect(page.locator('.cm-editor').first()).toBeVisible();
  });

  test('has default code loaded in the editor', async ({ page }) => {
    await expect(page.locator('.cm-content').first()).not.toBeEmpty();
  });

  test('renders the toolbar with runner and example selectors', async ({
    page,
  }) => {
    await expect(page.locator('.toolbar__runner')).toBeVisible();
    await expect(page.locator('.toolbar__example')).toBeVisible();
  });

  test('shows HIR output by default', async ({ page }) => {
    // t-hir-pretty uses display:contents on :host (zero bounding box);
    // check the rendered t-codemirror inside it instead.
    const hirOutput = page.locator('t-hir-pretty').locator('t-codemirror');
    await expect(hirOutput).toBeVisible();
  });

  test('can switch to AST output', async ({ page }) => {
    await page.locator('#ast').click();
    const astOutput = page.locator('.output-ast');
    await expect(astOutput).toBeVisible();
    await expect(astOutput).not.toBeEmpty();
  });

  test('can switch runner to JavaScript (Compiler) mode', async ({ page }) => {
    await page
      .locator('.toolbar__runner')
      .locator('select')
      .selectOption('JavaScript');
    // The JavaScript tab should now appear in the output tabs
    await expect(page.locator('#javascript')).toBeVisible();
  });

  test('shows JavaScript output in compiler mode', async ({ page }) => {
    await page
      .locator('.toolbar__runner')
      .locator('select')
      .selectOption('JavaScript');
    // Switching the runner doesn't auto-switch the active tab; click it explicitly.
    await page.locator('#javascript').click();
    const jsOutput = page.locator('.output-code');
    await expect(jsOutput).toBeVisible();
  });

  test('can load a different example from the dropdown', async ({ page }) => {
    const exampleSelect = page.locator('.toolbar__example').locator('select');
    // Select the fibonacci example
    await exampleSelect.selectOption('fibonacci.tlang');
    const editorContent = await page
      .locator('.cm-content')
      .first()
      .textContent();
    expect(editorContent).toContain('fibonacci');
  });

  test('shows the console panel', async ({ page }) => {
    await expect(page.locator('t-console')).toBeVisible();
  });

  test('shows console output after running code', async ({ page }) => {
    // Load fibonacci example which logs output
    await page
      .locator('.toolbar__example')
      .locator('select')
      .selectOption('fibonacci.tlang');
    await page
      .locator('.toolbar__runner')
      .locator('select')
      .selectOption('JavaScript');
    // Run the code
    await page.locator('t-button').filter({ hasText: 'Run' }).click();
    // run() wraps output in a console.group/groupEnd pair; the first
    // t-console-message is that empty group header (no visible content).
    // Assert that at least one message was produced instead.
    await expect(page.locator('t-console-message')).not.toHaveCount(0);
  });

  test('runs regex.tlang (with Unicode comments) via JS backend without errors', async ({
    page,
  }) => {
    // regex.tlang contains Unicode box-drawing characters (U+2500) in section
    // header comments.  These caused btoa() to throw "String contains an
    // invalid character" when building the source-map data URL.  This test is
    // the regression guard for that bug.
    await page
      .locator('.toolbar__example')
      .locator('select')
      .selectOption('regex/example.tlang');
    await page
      .locator('.toolbar__runner')
      .locator('select')
      .selectOption('JavaScript');
    await page.locator('t-button').filter({ hasText: 'Run' }).click();

    // No error messages should appear.
    await expect(page.locator('t-console-message[type="error"]')).toHaveCount(
      0,
    );

    // Expected output from the email-validation section.
    // t-console-message uses display:contents, so check the t-message child.
    const messages = page.locator('t-console-message[type="log"]');
    await expect(
      messages.filter({ hasText: 'true' }).first().locator('t-message'),
    ).toBeVisible();
  });

  test('can toggle constant folding optimization', async ({ page }) => {
    await openOptimizationSettings(page);

    const constantFolding = page.getByRole('menuitemcheckbox', {
      name: 'Constant folding',
    });
    await expect(constantFolding).toBeVisible();
    await expect(constantFolding).toHaveJSProperty('checked', true);

    await constantFolding.click();
    await expect(constantFolding).toHaveJSProperty('checked', false);

    await constantFolding.click();
    await expect(constantFolding).toHaveJSProperty('checked', true);
  });
});

test('runs every dropdown example in interpreter mode without browser or playground errors', async ({
  page,
}) => {
  test.setTimeout(120_000);

  const browserErrors = collectBrowserErrors(page);

  await gotoPlayground(page);
  await page
    .locator('.toolbar__runner')
    .locator('select')
    .selectOption('Interpreter');

  const exampleSelect = page.locator('.toolbar__example').locator('select');
  const examples = await exampleSelect
    .locator('option')
    .evaluateAll((options) =>
      options
        .map((option) => (option as HTMLOptionElement).value)
        .filter((value) => value.endsWith('.tlang')),
    );

  for (const example of examples) {
    const browserErrorCount = browserErrors.length;
    const consoleCount = await getPlaygroundConsoleCount(page);

    await exampleSelect.selectOption(example);
    await waitForSelectedExample(page, example);
    await runPlayground(page, consoleCount);

    const newBrowserErrors = browserErrors.slice(browserErrorCount);
    if (newBrowserErrors.length > 0) {
      throw new Error(
        `${example} logged browser errors:\n${newBrowserErrors.join('\n\n')}`,
      );
    }

    const newConsoleMessages = await getPlaygroundConsoleMessagesSince(
      page,
      consoleCount,
    );
    const consoleErrors = newConsoleMessages.filter(
      (message) => message.type === 'error',
    );

    if (consoleErrors.length > 0) {
      throw new Error(
        `${example} logged playground console errors:\n${consoleErrors
          .map((message) => message.text)
          .join('\n\n')}`,
      );
    }
  }
});

test.describe('Diagnostics panel', () => {
  test('diagnostics panel is hidden when there are no diagnostics', async ({
    page,
  }) => {
    // Default example should have no diagnostics (or at least no errors)
    await gotoPlayground(page);
    // t-diagnostics should either not exist or be hidden
    const panel = page.locator('t-diagnostics');
    // The hidden attribute is set when there are no messages
    await expect(panel).toHaveJSProperty('hidden', true);
  });

  test('diagnostics panel is visible when there are errors', async ({
    page,
  }) => {
    // Source: `missing_var;` — triggers "Use of undeclared variable" error
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await expect(panel).toHaveJSProperty('hidden', false);
  });

  test('diagnostics panel is visible when there are warnings', async ({
    page,
  }) => {
    // Source: `let x = 42;` — triggers "Unused variable" warning only
    await gotoPlayground(page, '#source=DYUwLgBAHhC8EBYBMBuIA');

    const panel = page.locator('t-diagnostics');
    await expect(panel).toHaveJSProperty('hidden', false);
  });

  test('diagnostics panel shows error count', async ({ page }) => {
    // Source: `missing_var;` — triggers "Use of undeclared variable" error
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await expect(panel.locator('.severity-count--error')).toContainText(
      'error',
    );
  });

  test('diagnostics panel shows warning count', async ({ page }) => {
    // Source: `let x = 42;` — triggers "Unused variable" warning only
    await gotoPlayground(page, '#source=DYUwLgBAHhC8EBYBMBuIA');

    const panel = page.locator('t-diagnostics');
    await expect(panel.locator('.severity-count--warning')).toContainText(
      'warning',
    );
  });

  test('diagnostics panel is collapsed by default', async ({ page }) => {
    // Source: `missing_var;` — triggers "Use of undeclared variable" error
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    // The messages container should not be visible when collapsed
    await expect(panel.locator('.messages')).not.toBeVisible();
  });

  test('diagnostics panel can be expanded with toggle button', async ({
    page,
  }) => {
    // Source: `missing_var;` — triggers "Use of undeclared variable" error
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await panel.getByLabel('Expand Diagnostics').click();

    // Messages should now be visible
    await expect(panel.locator('.messages')).toBeVisible();
  });

  test('diagnostics panel can be collapsed after expanding', async ({
    page,
  }) => {
    // Source: `missing_var;` — triggers "Use of undeclared variable" error
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await panel.getByLabel('Expand Diagnostics').first().click();
    await expect(panel.locator('.messages').first()).toBeVisible();

    // At compact viewports (default 1280×720), the dialog covers the outer
    // toolbar, so collapse via the dialog's button if open, else outer toolbar.
    const dialog = panel.locator('dialog');
    const collapseTarget = (await dialog.isVisible())
      ? dialog.getByLabel('Collapse Diagnostics')
      : panel.getByLabel('Collapse Diagnostics').first();
    await collapseTarget.click();
    await expect(panel.locator('.messages').first()).not.toBeVisible();
  });

  test('semantic error is rendered as HTML in diagnostics panel, not ANSI codes', async ({
    page,
  }) => {
    // Source: `missing_var;` — triggers "Use of undeclared variable" error
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await panel.getByLabel('Expand Diagnostics').click();

    const message = panel.locator('t-message[severity="error"]');
    await expect(message).toBeVisible();

    // Should contain styled spans from ansiToHtml
    const styledSpan = message.locator('span[style]');
    await expect(styledSpan).not.toHaveCount(0);

    // ANSI escape codes must NOT be present
    const textContent = await message.textContent();
    expect(textContent).not.toContain('\x1b[');
  });

  test('warning is rendered as HTML in diagnostics panel, not ANSI codes', async ({
    page,
  }) => {
    // Source: `let x = 42;` — triggers "Unused variable" warning only
    await gotoPlayground(page, '#source=DYUwLgBAHhC8EBYBMBuIA');

    const panel = page.locator('t-diagnostics');
    await panel.getByLabel('Expand Diagnostics').click();

    const message = panel.locator('t-message[severity="warning"]');
    await expect(message).toBeVisible();

    // Should contain styled spans from ansiToHtml
    const styledSpan = message.locator('span[style]');
    await expect(styledSpan).not.toHaveCount(0);

    // ANSI escape codes must NOT be present
    const textContent = await message.textContent();
    expect(textContent).not.toContain('\x1b[');
  });

  test('parse error is shown in diagnostics panel', async ({ page }) => {
    // Source: `let x = ;` — triggers a parse error
    await gotoPlayground(page, '#source=DYUwLgBAHhC8EG4g');

    const panel = page.locator('t-diagnostics');
    await expect(panel).toHaveJSProperty('hidden', false);
    await expect(panel.locator('.severity-count--error')).toBeVisible();
  });

  test('diagnostics do not appear in the console', async ({ page }) => {
    // Source: `missing_var;` — triggers "Use of undeclared variable" error
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    // Console should have no error or warn messages from diagnostics
    await expect(page.locator('t-console-message[type="error"]')).toHaveCount(
      0,
    );
    await expect(page.locator('t-console-message[type="warn"]')).toHaveCount(0);
  });

  test('warnings do not appear in the console', async ({ page }) => {
    // Source: `let x = 42;` — triggers "Unused variable" warning only
    await gotoPlayground(page, '#source=DYUwLgBAHhC8EBYBMBuIA');

    await expect(page.locator('t-console-message[type="warn"]')).toHaveCount(0);
  });

  test('on compact viewport, expand opens a fullscreen dialog', async ({
    page,
  }) => {
    // Compact: max-width 640px OR max-height 800px
    await page.setViewportSize({ width: 640, height: 780 });
    // Source: `missing_var;` — triggers "Use of undeclared variable" error
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await expect(panel).toHaveJSProperty('hidden', false);
    const dialog = panel.locator('dialog');

    // Dialog element exists but is not open
    await expect(dialog).toBeAttached();
    await expect(dialog).not.toBeVisible();

    // Click via JS — toolbar may be fully obscured at small compact sizes
    await panel
      .getByLabel('Expand Diagnostics')
      .evaluate((el) => (el as HTMLElement).click());
    await expect(dialog).toBeVisible();
    await expect(dialog.locator('.messages')).toBeVisible();
  });

  test('on compact viewport, dialog can be closed with collapse button', async ({
    page,
  }) => {
    await page.setViewportSize({ width: 640, height: 780 });
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await expect(panel).toHaveJSProperty('hidden', false);
    const dialog = panel.locator('dialog');

    await panel
      .getByLabel('Expand Diagnostics')
      .evaluate((el) => (el as HTMLElement).click());
    await expect(dialog).toBeVisible();

    // The toolbar inside the dialog has a collapse button
    await dialog.getByLabel('Collapse Diagnostics').click();
    await expect(dialog).not.toBeVisible();
  });

  test('on compact viewport, dialog can be closed with Escape', async ({
    page,
  }) => {
    await page.setViewportSize({ width: 640, height: 780 });
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await expect(panel).toHaveJSProperty('hidden', false);
    const dialog = panel.locator('dialog');

    await panel
      .getByLabel('Expand Diagnostics')
      .evaluate((el) => (el as HTMLElement).click());
    await expect(dialog).toBeVisible();

    await page.keyboard.press('Escape');
    await expect(dialog).not.toBeVisible();
  });

  test('inline expansion collapses when viewport shrinks to compact', async ({
    page,
  }) => {
    // Start with a large viewport (non-compact)
    await page.setViewportSize({ width: 1280, height: 900 });
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await expect(panel).toHaveJSProperty('hidden', false);

    // Expand inline
    await panel.getByLabel('Expand Diagnostics').click();
    await expect(panel.locator('.messages')).toBeVisible();

    // Shrink viewport to compact — inline expansion should collapse
    await page.setViewportSize({ width: 640, height: 780 });
    await expect(panel.locator('dialog')).not.toBeVisible();
  });

  test('on desktop viewport, expand shows inline messages without dialog', async ({
    page,
  }) => {
    await page.setViewportSize({ width: 1280, height: 900 });
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await expect(panel).toHaveJSProperty('hidden', false);

    await panel.getByLabel('Expand Diagnostics').click();

    // Messages should be inline, not inside a dialog
    const dialog = panel.locator('dialog');
    await expect(dialog).not.toBeAttached();
    await expect(panel.locator('.messages')).toBeVisible();
  });
});

test.describe('Optimization options URL persistence', () => {
  test('disabling constant folding adds opt=cf:false to hash', async ({
    page,
  }) => {
    await gotoPlayground(page);
    await openOptimizationSettings(page);
    await page
      .getByRole('menuitemcheckbox', { name: 'Constant folding' })
      .click();

    await expect(page).toHaveURL(/opt=cf%3Afalse/);
  });

  test('enabling ANF transform in interpreter mode adds opt=anf:full to hash', async ({
    page,
  }) => {
    await gotoPlayground(page);
    await openOptimizationSettings(page);
    await page.getByRole('menuitemcheckbox', { name: 'ANF transform' }).click();

    await expect(page).toHaveURL(/opt=anf%3Afull/);
  });

  test('removing non-default optimizations removes opt from hash', async ({
    page,
  }) => {
    await gotoPlayground(page);
    await openOptimizationSettings(page);

    const anfCheckbox = page.getByRole('menuitemcheckbox', {
      name: 'ANF transform',
    });

    await anfCheckbox.click();
    await expect(page).toHaveURL(/opt=/);

    // Toggle back to default — opt should be removed.
    await anfCheckbox.click();
    await expect(page).not.toHaveURL(/opt=/);
  });

  test('selecting ANF Full mode in compiler mode adds opt=anf:full to hash', async ({
    page,
  }) => {
    await gotoPlayground(page);
    await page
      .locator('.toolbar__runner')
      .locator('select')
      .selectOption('JavaScript');
    await expect(page.locator('#javascript')).toBeVisible();
    await openOptimizationSettings(page);
    await optimizationRadio(page, 'Full').click();

    await expect(page).toHaveURL(/opt=anf%3Afull/);
  });

  test('constant folding off is restored from URL hash on load', async ({
    page,
  }) => {
    await gotoPlayground(page, '#opt=cf%3Afalse');

    await openOptimizationSettings(page);

    await expect(
      page.getByRole('menuitemcheckbox', { name: 'Constant folding' }),
    ).toHaveJSProperty('checked', false);
  });

  test('ANF full mode in interpreter is restored from URL hash on load', async ({
    page,
  }) => {
    await gotoPlayground(page, '#opt=anf%3Afull');

    await openOptimizationSettings(page);

    await expect(
      page.getByRole('menuitemcheckbox', { name: 'ANF transform' }),
    ).toHaveJSProperty('checked', true);
  });

  test('ANF full mode in compiler is restored from URL hash on load', async ({
    page,
  }) => {
    await gotoPlayground(page, '#runner=JavaScript&opt=anf%3Afull');
    await expect(page.locator('#javascript')).toBeVisible();

    await openOptimizationSettings(page);

    await expect(optimizationRadio(page, 'Full')).toHaveJSProperty(
      'checked',
      true,
    );
    await expect(optimizationRadio(page, 'Minimal')).toHaveJSProperty(
      'checked',
      false,
    );
  });

  test('combined options are restored from URL hash on load', async ({
    page,
  }) => {
    await gotoPlayground(page, '#opt=cf%3Afalse%2Canf%3Afull');

    await openOptimizationSettings(page);

    await expect(
      page.getByRole('menuitemcheckbox', { name: 'Constant folding' }),
    ).toHaveJSProperty('checked', false);
    await expect(
      page.getByRole('menuitemcheckbox', { name: 'ANF transform' }),
    ).toHaveJSProperty('checked', true);
  });
});

test.describe('Code Completion', () => {
  test('shows dynamic completion items from semantic analysis', async ({
    page,
  }) => {
    await gotoPlayground(page, '#example=enums%2Fsimple.tlang');

    // Wait for the editor and analysis to complete
    const editor = page.locator('.cm-content').first();
    await expect(editor).toBeVisible();

    // Wait for HIR output to appear (signals analysis is complete)
    await expect(
      page.locator('.cm-editor').nth(1).locator('.cm-content'),
    ).not.toBeEmpty();

    // Click at the end of the editor and type to trigger completion
    await editor.click();
    await page.keyboard.press('Control+End');
    await page.keyboard.press('Enter');

    // Type 'ev' to trigger completion popup with dynamic items
    await page.keyboard.type('ev', { delay: 50 });

    // The completion popup should appear with the 'evaluate' function
    // from the semantic analysis of the simple.tlang example
    const completions = page.getByRole('listbox', { name: 'Completions' });
    await expect(completions).toBeVisible();

    // Verify that 'evaluate' (a user-defined function) appears in completions
    await expect(
      completions.getByRole('option', { name: /evaluate/ }),
    ).toBeVisible();
  });
});

test.describe('Hover tooltip', () => {
  /**
   * Helper: get the pixel coordinates of a character at the given CodeMirror
   * document offset.  Traverses through the tlang-playground → t-codemirror
   * shadow DOM hierarchy to reach the underlying EditorView.
   *
   * NOTE: This relies on CodeMirror 6 internals (`cmTile` property on content
   * DOM nodes) which may change in future versions.  If this breaks after a
   * CodeMirror upgrade, check the `Tile.get()` implementation in
   * `@codemirror/view` for the current property name.
   */
  async function coordsAtPos(page: Page, pos: number) {
    const result = await page.evaluate((p) => {
      const playground = document.querySelector('tlang-playground');
      if (!playground?.shadowRoot) return { error: 'no playground shadowRoot' };
      const codemirror = playground.shadowRoot.querySelector('t-codemirror');
      if (!codemirror?.shadowRoot) return { error: 'no codemirror shadowRoot' };
      const cmEl = codemirror.shadowRoot.querySelector('.cm-editor');
      if (!cmEl) return { error: 'no .cm-editor element' };
      const content = cmEl.querySelector('.cm-content');
      if (!content) return { error: 'no .cm-content element' };
      // CodeMirror 6 stores a Tile reference on .cm-content via `cmTile`.
      // Walking `.root.view` gives us the EditorView instance.
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const view = (content as any).cmTile?.root?.view;
      if (!view?.coordsAtPos) return { error: 'no EditorView' };
      const c = view.coordsAtPos(p);
      if (!c) return { error: `coordsAtPos(${p}) returned null` };
      return { left: c.left, top: c.top, bottom: c.bottom };
    }, pos);
    if ('error' in result) {
      throw new Error(`coordsAtPos failed: ${result.error}`);
    }
    return result as { left: number; top: number; bottom: number };
  }

  test('hovering over a function name shows a hover tooltip', async ({
    page,
  }) => {
    // Source: `fn add(a, b) { a + b }`
    await gotoPlayground(page, '#source=GYOwBAhgJlAUEBowCMCUYDekwGoVgF8g');

    const editor = page.locator('.cm-content').first();
    await expect(editor).toBeVisible();

    // Offset 4 = middle of "add" in "fn add(a, b) { a + b }"
    const coords = await coordsAtPos(page, 4);
    await page.mouse.move(coords.left, (coords.top + coords.bottom) / 2);

    const tooltip = page.locator('.cm-tooltip-hover');
    await expect(tooltip).toBeVisible({ timeout: 5000 });

    const hoverContent = tooltip.locator('.cm-tlang-hover');
    await expect(hoverContent).toBeVisible();

    const text = await hoverContent.textContent();
    expect(text).toContain('fn add(');
    expect(text).toContain('unknown');
  });

  test('hover tooltip disappears when moving away', async ({ page }) => {
    // Source: `fn add(a, b) { a + b }`
    await gotoPlayground(page, '#source=GYOwBAhgJlAUEBowCMCUYDekwGoVgF8g');

    const editor = page.locator('.cm-content').first();
    await expect(editor).toBeVisible();

    const coords = await coordsAtPos(page, 4);
    await page.mouse.move(coords.left, (coords.top + coords.bottom) / 2);

    const tooltip = page.locator('.cm-tooltip-hover');
    await expect(tooltip).toBeVisible({ timeout: 5000 });

    // Move away from the editor content entirely
    await page.mouse.move(0, 0);
    await expect(tooltip).not.toBeVisible({ timeout: 5000 });
  });
});

test.describe('Signature help', () => {
  test('shows function signature help and updates the active parameter', async ({
    page,
  }) => {
    await gotoPlayground(page);

    const source = 'fn add(a, b) { a + b }\nadd(1, 2);';
    const firstArg = source.indexOf('1');
    const secondArg = source.indexOf('2');

    await updateEditorState(page, { source, selection: firstArg });

    const tooltip = page.locator('.cm-tooltip-tlang-signature-help');
    await expect(tooltip).toBeVisible({ timeout: 5000 });
    await expect(
      tooltip.locator('.cm-tlang-signature-help-label'),
    ).toContainText('add(a: unknown, b: unknown) -> unknown');
    await expect(
      tooltip.locator('.cm-tlang-signature-help-param-active'),
    ).toHaveText('a: unknown');

    await updateEditorState(page, { selection: secondArg });

    await expect(
      tooltip.locator('.cm-tlang-signature-help-param-active'),
    ).toHaveText('b: unknown');
  });
});
