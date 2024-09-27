import { TlangCompiler } from './tlang';

const cli_examples = Object.fromEntries(
  Object.entries(
    import.meta.glob('../../crates/tlang_cli_js/examples/*.tl', {
      query: '?raw',
      import: 'default',
      eager: true,
    }),
  ).map(([path, source]) => [path.split('/').pop()!, String(source)]),
);

export const examples: Record<string, string> = {
  ...cli_examples,
  'stdlib.tl': TlangCompiler.standardLibrarySource,
};
