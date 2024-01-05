export const examples = Object.fromEntries(
  Object.entries(import.meta.glob('../../crates/tlang_cli_js/examples/*.tl', { as: 'raw', eager: true }))
    .map(([path, source]) => [path.split('/').pop()!, source])
)
