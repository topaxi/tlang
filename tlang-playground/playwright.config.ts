import { defineConfig, devices } from '@playwright/test';

const isCi = !!process.env['CI'];
const isGithubActions = !!process.env['GITHUB_ACTIONS'];

export default defineConfig({
  testDir: './e2e',
  fullyParallel: true,
  forbidOnly: isCi,
  retries: 0,
  workers: isCi ? 1 : undefined,
  timeout: isCi ? 15_000 : 30_000,
  expect: {
    timeout: isCi ? 2_500 : 5_000,
  },
  reporter: isGithubActions ? 'github' : 'line',
  use: {
    baseURL: 'http://localhost:4173/tlang',
    trace: 'on-first-retry',
  },
  projects: [
    {
      name: 'chromium',
      use: { ...devices['Desktop Chrome'] },
    },
  ],
  webServer: {
    command: 'npm run test:e2e:webserver',
    url: 'http://localhost:4173/tlang',
    reuseExistingServer: !isCi,
    timeout: isCi ? 10_000 : 60_000,
  },
});
