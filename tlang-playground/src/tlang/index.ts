import init, {
  getStandardLibrarySource,
  getStandardLibraryNativeJs,
  Tlang,
} from 'tlang_bindings_js';

await init();

export {
  Tlang,
  type CodemirrorDiagnostic,
  type Runner,
  type JsHirPrettyOptions,
  type JsOptimizationOptions,
  type JsDiagnostic,
  type JsParseIssue,
  type JsSpan,
  type JsLineColumn,
  type JsSeverity,
  type JsParseIssueKind,
} from 'tlang_bindings_js';

export const standardLibrarySource = getStandardLibrarySource();

let standardLibraryCompiled = '';

export function getStandardLibraryCompiled(): string {
  if (standardLibraryCompiled !== '') {
    return standardLibraryCompiled;
  }

  const compiler = new Tlang(standardLibrarySource, 'JavaScript');

  standardLibraryCompiled =
    compiler.getJavaScript() + '\n' + getStandardLibraryNativeJs();

  compiler.free();

  return standardLibraryCompiled;
}
