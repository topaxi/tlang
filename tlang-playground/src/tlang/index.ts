import init, { getStandardLibrarySource, Tlang } from 'tlang_bindings_js';

await init();

export {
  Tlang,
  type CodemirrorDiagnostic,
  type Runner,
} from 'tlang_bindings_js';

export const standardLibrarySource = getStandardLibrarySource();

let standardLibraryCompiled = '';

export function getStandardLibraryCompiled(): string {
  if (standardLibraryCompiled !== '') {
    return standardLibraryCompiled;
  }

  const compiler = new Tlang(standardLibrarySource, 'JavaScript');

  compiler.compileToJS();
  const output = compiler.getJavaScript();

  compiler.free();

  standardLibraryCompiled = output;

  return standardLibraryCompiled;
}
