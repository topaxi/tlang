import { standardLibrarySource } from './tlang';

const cli_examples = Object.fromEntries(
  Object.entries(
    import.meta.glob('../../examples/*.tlang', {
      query: '?raw',
      import: 'default',
      eager: true,
    }),
  ).map(([path, source]) => [path.split('/').pop()!, String(source)]),
);

export const examples: Record<string, string> = {
  ...cli_examples,
  'stdlib.tl': standardLibrarySource,
};
