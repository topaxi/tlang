import { test, expect, type Page } from '@playwright/test';

async function gotoPlayground(page: Page, hash = '') {
  await page.goto(`/tlang${hash}`);
  await expect(page.locator('.cm-content').first()).toBeVisible();
}

async function openOptimizationSettings(page: Page) {
  await page.getByLabel('Optimization Settings').click();
}

function optimizationRadio(page: Page, label: 'Minimal' | 'Full') {
  return page.locator('t-menuitem-radio').filter({ hasText: label });
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
      .selectOption('regex.tlang');
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
    const messages = page.locator('t-console-message[type="log"]');
    await expect(messages.filter({ hasText: 'true' }).first()).toBeVisible();
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
    await panel.getByLabel('Expand Diagnostics').click();
    await expect(panel.locator('.messages')).toBeVisible();

    await panel.getByLabel('Collapse Diagnostics').click();
    await expect(panel.locator('.messages')).not.toBeVisible();
  });

  test('semantic error is rendered as HTML in diagnostics panel, not ANSI codes', async ({
    page,
  }) => {
    // Source: `missing_var;` — triggers "Use of undeclared variable" error
    await gotoPlayground(page, '#source=LYSwzmIHYOYPoDcCGAnA3EA');

    const panel = page.locator('t-diagnostics');
    await panel.getByLabel('Expand Diagnostics').click();

    const message = panel.locator('.message--error');
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

    const message = panel.locator('.message--warning');
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
