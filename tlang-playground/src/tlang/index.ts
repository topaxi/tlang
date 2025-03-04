import init, { getStandardLibrarySource, Tlang } from 'tlang_bindings_js';

await init();

export { Tlang, type CodemirrorDiagnostic } from 'tlang_bindings_js';

export const standardLibrarySource = getStandardLibrarySource();

let standardLibraryCompiled = '';

export function getStandardLibraryCompiled(): string {
  if (standardLibraryCompiled !== '') {
    return standardLibraryCompiled;
  }

  const compiler = new Tlang(standardLibrarySource);

  compiler.compileToJS();
  const output = compiler.js;

  compiler.free();

  standardLibraryCompiled = output;

  return standardLibraryCompiled;
}
