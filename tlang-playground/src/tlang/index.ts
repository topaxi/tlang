import init, {
  getStandardLibrarySource,
  TlangCompiler,
} from 'tlang_bindings_js';

export { type CodemirrorDiagnostic, TlangInterpreter } from 'tlang_bindings_js';

await init();

export const standardLibrarySource = getStandardLibrarySource();

let standardLibraryCompiled = '';

export function getStandardLibraryCompiled(): string {
  if (standardLibraryCompiled !== '') {
    return standardLibraryCompiled;
  }

  const compiler = compile(standardLibrarySource);

  const { output } = compiler;

  compiler.free();

  standardLibraryCompiled = output;

  return standardLibraryCompiled;
}

export function compile(str: string): TlangCompiler {
  let compiler = new TlangCompiler(str);

  compiler.compile();

  return compiler;
}

export { type TlangCompiler };
