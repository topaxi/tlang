import init, {
  getStandardLibraryModule,
  getStandardLibrarySource,
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

export function getStandardLibraryCompiled(): string {
  return getStandardLibraryModule();
}
