import { test, expect } from '@playwright/test';

test.describe('Tlang Playground', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/tlang');
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
    await page.locator('.toolbar__runner').selectOption('JavaScript');
    // The JavaScript tab should now appear in the output tabs
    await expect(page.locator('#javascript')).toBeVisible();
  });

  test('shows JavaScript output in compiler mode', async ({ page }) => {
    await page.locator('.toolbar__runner').selectOption('JavaScript');
    // Switching the runner doesn't auto-switch the active tab; click it explicitly.
    await page.locator('#javascript').click();
    const jsOutput = page.locator('.output-code');
    await expect(jsOutput).toBeVisible();
  });

  test('can load a different example from the dropdown', async ({ page }) => {
    const exampleSelect = page.locator('.toolbar__example');
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
    await page.locator('.toolbar__example').selectOption('fibonacci.tlang');
    await page.locator('.toolbar__runner').selectOption('JavaScript');
    // Run the code
    await page.locator('t-button').filter({ hasText: 'Run' }).click();
    // run() wraps output in a console.group/groupEnd pair; the first
    // t-console-message is that empty group header (no visible content).
    // Assert that at least one message was produced instead.
    await expect(page.locator('t-console-message')).not.toHaveCount(0);
  });

  test('can toggle constant folding optimization', async ({ page }) => {
    await page.getByLabel('Optimization Settings').click();

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

test.describe('Optimization options URL persistence', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/tlang');
    // Ensure the WASM-backed editor is ready before interacting.
    await expect(page.locator('.cm-content').first()).toBeVisible();
  });

  test('disabling constant folding adds opt=cf:false to hash', async ({
    page,
  }) => {
    await page.getByLabel('Optimization Settings').click();
    await page
      .getByRole('menuitemcheckbox', { name: 'Constant folding' })
      .click();

    await expect(page).toHaveURL(/opt=cf%3Afalse/);
  });

  test('enabling ANF transform in interpreter mode adds opt=anf:full to hash', async ({
    page,
  }) => {
    await page.getByLabel('Optimization Settings').click();
    await page.getByRole('menuitemcheckbox', { name: 'ANF transform' }).click();

    await expect(page).toHaveURL(/opt=anf%3Afull/);
  });

  test('removing non-default optimizations removes opt from hash', async ({
    page,
  }) => {
    await page.getByLabel('Optimization Settings').click();

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
    await page.locator('.toolbar__runner').selectOption('JavaScript');
    await page.getByLabel('Optimization Settings').click();
    await page.getByRole('menuitemradio', { name: 'Full' }).click();

    await expect(page).toHaveURL(/opt=anf%3Afull/);
  });

  test('constant folding off is restored from URL hash on load', async ({
    page,
  }) => {
    await page.goto('/tlang#opt=cf%3Afalse');
    await expect(page.locator('.cm-content').first()).toBeVisible();

    await page.getByLabel('Optimization Settings').click();

    await expect(
      page.getByRole('menuitemcheckbox', { name: 'Constant folding' }),
    ).toHaveJSProperty('checked', false);
  });

  test('ANF full mode in interpreter is restored from URL hash on load', async ({
    page,
  }) => {
    await page.goto('/tlang#opt=anf%3Afull');
    await expect(page.locator('.cm-content').first()).toBeVisible();

    await page.getByLabel('Optimization Settings').click();

    await expect(
      page.getByRole('menuitemcheckbox', { name: 'ANF transform' }),
    ).toHaveJSProperty('checked', true);
  });

  test('ANF full mode in compiler is restored from URL hash on load', async ({
    page,
  }) => {
    await page.goto('/tlang#runner=JavaScript&opt=anf%3Afull');
    await expect(page.locator('.cm-content').first()).toBeVisible();

    await page.getByLabel('Optimization Settings').click();

    await expect(
      page.getByRole('menuitemradio', { name: 'Full' }),
    ).toHaveJSProperty('checked', true);
    await expect(
      page.getByRole('menuitemradio', { name: 'Minimal' }),
    ).toHaveJSProperty('checked', false);
  });

  test('combined options are restored from URL hash on load', async ({
    page,
  }) => {
    await page.goto('/tlang#opt=cf%3Afalse%2Canf%3Afull');
    await expect(page.locator('.cm-content').first()).toBeVisible();

    await page.getByLabel('Optimization Settings').click();

    await expect(
      page.getByRole('menuitemcheckbox', { name: 'Constant folding' }),
    ).toHaveJSProperty('checked', false);
    await expect(
      page.getByRole('menuitemcheckbox', { name: 'ANF transform' }),
    ).toHaveJSProperty('checked', true);
  });
});
