import {
  CodemirrorDiagnostic,
  getStandardLibraryCompiled,
  Tlang,
  type JsHirPrettyOptions,
  type JsOptimizationOptions,
  type Runner,
} from '../tlang';
import { ConsoleMessage, createConsoleMessage } from '../components/t-console';
import {
  type ParsedSourceMap,
  parseSourceMap,
  remapStack,
  shiftSourceMapLines,
} from '../utils/source-map';

type CodemirrorSeverity = 'hint' | 'info' | 'warning' | 'error';

export interface LocalDiagnostic {
  from: number;
  to: number;
  message: string;
  severity: CodemirrorSeverity;
}

export class TlangController {
  private tlang: Tlang;
  consoleMessages: ConsoleMessage[] = [];
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
  private prefixLines = 0;
  private optimizationOptions: JsOptimizationOptions = {
    constantFolding: true,
    anfTransform: undefined,
  };

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
    this.analyze();
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
    this.prefixLines = 0;

    this.tlang = this.createTlang(source, runner);
    this.tlang.setOptimizations(this.optimizationOptions);
    this.analyze();
  }

  analyze(): CodemirrorDiagnostic[] {
    try {
      this.tlang.analyze();

      for (let diagnostic of this.tlang.getDiagnostics()) {
        let method =
          diagnostic.severity === 'error'
            ? ('error' as const)
            : ('warn' as const);
        let formatted = `${diagnostic.message} at ${diagnostic.span.start_lc.line}:${diagnostic.span.start_lc.column}`;

        this.logToConsole(method, formatted);
      }

      return this.tlang.getCodemirrorDiagnostics();
    } catch {
      for (let parseError of this.tlang.getParseErrors()) {
        let kind =
          typeof parseError.kind === 'string'
            ? parseError.kind
            : Object.keys(parseError.kind)[0];
        let formatted = `${kind}: ${parseError.msg} at ${parseError.span.start_lc.line}:${parseError.span.start_lc.column}`;

        this.logToConsole('error', formatted);
      }

      return [];
    }
  }

  run(runner: Runner) {
    this.logToConsole('group');
    let beforeOpenGroups = this.getConsoleOpenGroups();

    try {
      if (runner === 'JavaScript') {
        this.runCompiled();
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

  private runCompiled() {
    const bindings = {
      console: this.tlangConsole,
    };

    const bindingsDestructuring = `let {${Object.keys(bindings).join(',')}} = ___js_bindings;`;
    const stdlib = getStandardLibraryCompiled();

    // Compute the number of newlines in the preamble that precedes user code.
    // This is needed to shift the source map so generated-line numbers match
    // the actual lines inside the new Function body.
    const prefix = `${bindingsDestructuring}${stdlib}\n{`;
    const prefixLines = (prefix.match(/\n/g) ?? []).length;
    this.prefixLines = prefixLines;

    let sourceMappingComment = '\n//# sourceURL=tlang-script';

    const rawMap = this.tlang.getSourceMap?.();
    if (rawMap) {
      const shifted = shiftSourceMapLines(rawMap, prefixLines);
      this.cachedSourceMap = parseSourceMap(shifted);
      const encoded = btoa(shifted);
      sourceMappingComment += `\n//# sourceMappingURL=data:application/json;base64,${encoded}`;
    }

    const fn = new Function(
      '___js_bindings',
      `${prefix}${this.tlang.getJavaScript()}};${sourceMappingComment}`,
    );

    fn(bindings);
  }

  /** Remap an error's stack trace using the current source map, if available. */
  private remapError(error: Error): Error {
    if (!this.cachedSourceMap || !error.stack) return error;
    const remappedStack = remapStack(
      error.stack,
      this.cachedSourceMap,
      'tlang-script',
      this.prefixLines,
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
}
