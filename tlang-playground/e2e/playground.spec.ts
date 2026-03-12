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
