import init, { getStandardLibrarySource } from 'tlang_bindings_js';
import stdlibSource from 'tlang_bindings_js/stdlib.js?raw';

await init();

export {
  Tlang,
  type CodemirrorDiagnostic,
  type CodemirrorCompletion,
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

export function getStandardLibraryCompiled(): string {
  return stdlibSource;
}
