import {
  CodemirrorDiagnostic,
  getStandardLibraryCompiled,
  Tlang,
  type JsHirPrettyOptions,
  type Runner,
} from '../tlang';
import { ConsoleMessage, createConsoleMessage } from '../components/t-console';

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

  constructor(initialSource: string, initialRunner: Runner) {
    this.tlang = new Tlang(initialSource, initialRunner);
  }

  updateTlang(source: string, runner: Runner) {
    if (this.tlang) {
      this.tlang.free();
    }

    this.cachedAST = null;
    this.cachedHIR = null;
    this.cachedJS = null;

    this.tlang = new Tlang(source, runner);
    this.tlang.defineFunction('log', this.tlangConsole.log);

    for (let [method, fn] of Object.entries(this.tlangConsole)) {
      this.tlang.defineFunction(
        `log::${method}`,
        fn as (...args: unknown[]) => unknown,
      );
    }

    this.analyze();
  }

  analyze(): CodemirrorDiagnostic[] {
    try {
      this.tlang.analyze();

      for (let diagnostic of this.tlang.getDiagnostics()) {
        let method =
          diagnostic.severity === 'Error'
            ? ('error' as const)
            : ('warn' as const);
        let formatted = `${diagnostic.message} at ${diagnostic.span.start.line}:${diagnostic.span.start.column}`;

        this.logToConsole(method, formatted);
      }

      return this.tlang.getCodemirrorDiagnostics();
    } catch {
      for (let parseError of this.tlang.getParseErrors()) {
        let kind =
          typeof parseError.kind === 'string'
            ? parseError.kind
            : Object.keys(parseError.kind)[0];
        let formatted = `${kind}: ${parseError.msg} at ${parseError.span.start.line}:${parseError.span.start.column}`;

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
        this.logToConsole('error', error);
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
    let bindings = {
      console: this.tlangConsole,
      panic: (message: string) => {
        throw new Error(message);
      },
    };

    let bindingsDestructuring = `let {${Object.keys(bindings).join(',')}} = ___js_bindings;`;
    let fn = new Function(
      '___js_bindings',
      `${bindingsDestructuring}${getStandardLibraryCompiled()}\n{${this.tlang.getJavaScript()}};`,
    );

    fn(bindings);
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

  getHIRPretty(options?: JsHirPrettyOptions) {
    return this.tlang.getHIRPretty(options);
  }
}
