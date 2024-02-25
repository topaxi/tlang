export const examples = Object.fromEntries(
  Object.entries(
    import.meta.glob('../../crates/tlang_cli_js/examples/*.tl', {
      query: '?raw',
      import: 'default',
      eager: true,
    }),
  ).map(([path, source]) => [path.split('/').pop()!, String(source)]),
);
