import {
  CodemirrorDiagnostic,
  type CodemirrorCompletion,
  getStandardLibraryCompiled,
  Tlang,
  type JsHirPrettyOptions,
  type JsOptimizationOptions,
  type Runner,
} from '../tlang';
import { ConsoleMessage, createConsoleMessage } from '../components/t-console';
import { type DiagnosticMessage } from '../components/t-diagnostics';
import {
  type ParsedSourceMap,
  parseSourceMap,
  remapStack,
} from '../utils/source-map';
import { ansiToHtml } from '../utils/ansi-to-html';

export type { DiagnosticMessage };

type CodemirrorSeverity = 'hint' | 'info' | 'warning' | 'error';

interface StdlibModule {
  __exec: (code: string, console: object) => unknown;
}

export interface LocalDiagnostic {
  from: number;
  to: number;
  message: string;
  severity: CodemirrorSeverity;
}

export class TlangController {
  private tlang: Tlang;
  consoleMessages: ConsoleMessage[] = [];
  diagnosticMessages: DiagnosticMessage[] = [];
  private readonly tlangConsole = {
    log: (...args: unknown[]) => this.logToConsole('log', ...args),
    warn: (...args: unknown[]) => this.logToConsole('warn', ...args),
    error: (...args: unknown[]) => this.logToConsole('error', ...args),
    group: (...args: unknown[]) => this.logToConsole('group', ...args),
    groupEnd: (...args: unknown[]) => this.logToConsole('groupEnd', ...args),
  };

  private cachedAST: string | null = null;
  private cachedHIR: string | null = null;
  private cachedJS: string | null = null;
  private cachedJSAST: string | null = null;
  private cachedSourceMap: ParsedSourceMap | null = null;
  private cachedCompletionItems: CodemirrorCompletion[] = [];
  private optimizationOptions: JsOptimizationOptions = {
    constantFolding: true,
    anfTransform: undefined,
    anfReturnOpt: true,
    deadCodeElimination: true,
  };

  /**
   * Cached Promise for the stdlib Blob URL module.  Resolved once and reused
   * for every subsequent run so the stdlib is only compiled/loaded once.
   */
  private stdlibModulePromise: Promise<StdlibModule> | null = null;

  constructor(
    initialSource: string,
    initialRunner: Runner,
    initialOptimizationOptions?: JsOptimizationOptions,
  ) {
    this.tlang = this.createTlang(initialSource, initialRunner);
    if (initialOptimizationOptions) {
      this.optimizationOptions = initialOptimizationOptions;
      this.tlang.setOptimizations(initialOptimizationOptions);
    }
  }

  private createTlang(source: string, runner: Runner): Tlang {
    const tlang = new Tlang(source, runner);
    tlang.defineFunction('log', this.tlangConsole.log);

    for (let [method, fn] of Object.entries(this.tlangConsole)) {
      tlang.defineFunction(
        `log::${method}`,
        fn as (...args: unknown[]) => unknown,
      );
    }

    return tlang;
  }

  updateTlang(source: string, runner: Runner) {
    if (this.tlang) {
      this.tlang.free();
    }

    this.cachedAST = null;
    this.cachedHIR = null;
    this.cachedJS = null;
    this.cachedJSAST = null;
    this.cachedSourceMap = null;

    this.tlang = this.createTlang(source, runner);
    this.tlang.setOptimizations(this.optimizationOptions);
  }

  analyze(): CodemirrorDiagnostic[] {
    this.tlang.analyze();

    this.diagnosticMessages = [];

    const renderedParseErrors = this.tlang.renderParseErrors();
    if (renderedParseErrors) {
      this.diagnosticMessages.push({
        severity: 'error',
        html: ansiToHtml(renderedParseErrors),
      });
    }

    const renderedErrors = this.tlang.renderErrorDiagnostics();
    if (renderedErrors) {
      this.diagnosticMessages.push({
        severity: 'error',
        html: ansiToHtml(renderedErrors),
      });
    }

    const renderedWarnings = this.tlang.renderWarningDiagnostics();
    if (renderedWarnings) {
      this.diagnosticMessages.push({
        severity: 'warning',
        html: ansiToHtml(renderedWarnings),
      });
    }

    return this.tlang.getCodemirrorDiagnostics();
  }

  getCompletionItems(): CodemirrorCompletion[] {
    const items = this.tlang.getCompletionItems();

    const hasAnalysisFailures = Boolean(
      this.tlang.renderParseErrors() || this.tlang.renderErrorDiagnostics(),
    );

    if (hasAnalysisFailures) {
      return this.cachedCompletionItems;
    }

    this.cachedCompletionItems = items;

    return this.cachedCompletionItems;
  }

  async run(runner: Runner) {
    this.logToConsole('group');
    let beforeOpenGroups = this.getConsoleOpenGroups();

    try {
      if (runner === 'JavaScript') {
        await this.runCompiled();
      } else {
        this.runInterpreted();
      }
    } catch (error) {
      if (error instanceof Error) {
        this.logToConsole('error', this.remapError(error));
      }

      let openGroups = this.getConsoleOpenGroups();

      for (let i = beforeOpenGroups; i < openGroups; i++) {
        this.logToConsole('groupEnd');
      }
    }

    this.logToConsole('groupEnd');
  }

  clearConsole() {
    this.consoleMessages = [];
  }

  private logToConsole(method: ConsoleMessage['type'], ...args: unknown[]) {
    this.consoleMessages.push(createConsoleMessage(method, ...args));
  }

  private getConsoleOpenGroups() {
    return this.consoleMessages.filter((message) => message.type === 'group')
      .length;
  }

  /**
   * Load (or return the cached) stdlib ES module.
   *
   * The stdlib is wrapped in a Blob URL module that exports `__exec`.
   * `eval()` inside a module function has lexical access to the module scope,
   * so user code eval'd through `__exec` sees all stdlib names with zero
   * preamble offset — source maps work without any line shifting.
   */
  private getStdlibModule(): Promise<StdlibModule> {
    if (!this.stdlibModulePromise) {
      const stdlib = getStandardLibraryCompiled();
      const moduleCode =
        stdlib +
        '\nexport function __exec(code, ___console) {\n' +
        '  const console = ___console;\n' +
        '  return eval(code);\n' +
        '}';
      const blob = new Blob([moduleCode], { type: 'text/javascript' });
      const url = URL.createObjectURL(blob);
      this.stdlibModulePromise = import(
        /* @vite-ignore */ url
      ) as Promise<StdlibModule>;
    }
    return this.stdlibModulePromise;
  }

  private async runCompiled() {
    const mod = await this.getStdlibModule();

    const rawMap = this.tlang.getSourceMap?.();
    if (rawMap && !this.cachedSourceMap) {
      this.cachedSourceMap = parseSourceMap(rawMap);
    }

    let code = this.tlang.getJavaScript();
    code += '\n//# sourceURL=playground.tlang';
    if (rawMap) {
      // btoa() only accepts Latin-1; encode as UTF-8 bytes first so that
      // source files containing non-ASCII characters (e.g. Unicode comments)
      // don't throw "String contains an invalid character".
      const utf8Bytes = new TextEncoder().encode(rawMap);
      const latin1 = Array.from(utf8Bytes, (b) => String.fromCharCode(b)).join(
        '',
      );
      code += `\n//# sourceMappingURL=data:application/json;charset=utf-8;base64,${btoa(latin1)}`;
    }

    mod.__exec(code, this.tlangConsole);
  }

  /** Remap an error's stack trace using the current source map, if available. */
  private remapError(error: Error): Error {
    if (!this.cachedSourceMap || !error.stack) return error;
    const remappedStack = remapStack(
      error.stack,
      this.cachedSourceMap,
      'playground.tlang',
    );
    const remapped = new Error(error.message);
    remapped.stack = remappedStack;
    return remapped;
  }

  private runInterpreted() {
    this.tlang.eval();
  }

  getASTString() {
    return (this.cachedAST ??= this.tlang.getASTString());
  }

  getHIRString() {
    return (this.cachedHIR ??= this.tlang.getHIRString());
  }

  getJavaScript() {
    return (this.cachedJS ??= this.tlang.getJavaScript());
  }

  getJSASTString() {
    return (this.cachedJSAST ??= this.tlang.getJSASTString());
  }

  getHIRPretty(options?: JsHirPrettyOptions) {
    return this.tlang.getHIRPretty(options);
  }

  setOptimizations(options: JsOptimizationOptions) {
    this.optimizationOptions = options;
    this.cachedHIR = null;
    this.cachedJS = null;
    this.cachedJSAST = null;
    this.cachedSourceMap = null;
    this.tlang.setOptimizations(options);
  }

  getOptimizations(): JsOptimizationOptions {
    return this.optimizationOptions;
  }

  getHoverInfo(pos: number): { text: string; from: number; to: number } | null {
    return this.tlang.getHoverInfo(pos) ?? null;
  }

  getDefinitionLocation(pos: number): { from: number; to: number } | null {
    return this.tlang.getDefinitionLocation(pos) ?? null;
  }
}
