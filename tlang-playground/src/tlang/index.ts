import init, { TlangCompiler } from 'tlang_bindings_js';

await init();

let standardLibraryCompiled = ''

export const standardLibrarySource = TlangCompiler.standardLibrarySource;

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
