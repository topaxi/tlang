import globals from 'globals';
import eslint from '@eslint/js';
import tseslint from 'typescript-eslint';
import eslintPluginPrettierRecommended from 'eslint-plugin-prettier/recommended';

export default tseslint.config([
  { files: ['**/*.{js,mjs,cjs,ts}'] },
  {
    ignores: [
      '**/dist/',
      '**/wasm/',
      'packages/codemirror-lang-tlang/src/parser.js',
      'packages/codemirror-lang-tlang/src/parser.terms.js',
    ],
  },
  { languageOptions: { globals: globals.browser } },
  eslint.configs.recommended,
  tseslint.configs.recommended,
  eslintPluginPrettierRecommended,
  {
    rules: {
      'prefer-const': 'off',
      '@typescript-eslint/no-unused-vars': [
        'error',
        {
          args: 'all',
          argsIgnorePattern: '^_',
          caughtErrors: 'all',
          caughtErrorsIgnorePattern: '^_',
          destructuredArrayIgnorePattern: '^_',
          varsIgnorePattern: '^_',
          ignoreRestSiblings: true,
        },
      ],
    },
  },
]);
