import { LitElement, PropertyValueMap, css, html } from 'lit';
import { customElement, query, state } from 'lit/decorators.js';
import { examples } from './examples';

import './components/t-button';
import './components/t-codemirror';
import './components/t-console';
import './components/t-live';
import './components/t-shortcuts';
import './components/t-split';
import './components/t-tabs';
import { type TCodeMirror } from './components/t-codemirror';
import {
  ConsoleElement,
  ConsoleMessage,
  createConsoleMessage,
} from './components/t-console';
import { SplitElement, SplitEvent } from './components/t-split';
import { compressSource, decompressSource } from './utils/lz';
import {
  getStandardLibraryCompiled,
  Tlang,
  CodemirrorDiagnostic,
} from './tlang';
import { keyed } from 'lit/directives/keyed.js';
import { live } from 'lit/directives/live.js';
import { repeat } from 'lit/directives/repeat.js';
import { mediaQuery } from './decorators/media-query';

type CodemirrorSeverity = 'hint' | 'info' | 'warning' | 'error';

function getHashParams() {
  return new URLSearchParams(window.location.hash.slice(1));
}

// Default source code is either the code provided via hashcode "source"
// compressed by lz-string or the "example" hashcode with the corresponding
// example file name. Default is the first example.
async function defaultSource() {
  let params = getHashParams();

  if (params.has('source')) {
    return await decompressSource(params.get('source')!);
  } else if (params.has('example')) {
    let exampleName = params.get('example') ?? '';
    if (exampleName in examples) {
      return examples[exampleName];
    }
  }
  return examples[Object.keys(examples)[0]];
}

type OutputDisplay = 'ast' | 'hir' | 'hir_pretty' | 'javascript';

const displayLabels = {
  ast: 'AST',
  hir: 'HIR',
  hir_pretty: 'HIR (Pretty)',
  javascript: 'JavaScript',
} satisfies Record<OutputDisplay, string>;

function defaultDisplay(): OutputDisplay {
  let params = getHashParams();
  let display: OutputDisplay = 'hir_pretty';

  if (params.has('display')) {
    display = params.get('display') as OutputDisplay;
  }

  if (params.get('runner') === 'compiler') {
    return 'javascript';
  }

  if (params.get('runner') === 'interpreter' && display === 'javascript') {
    return 'hir_pretty';
  }

  return display;
}

function defaultExample() {
  let params = getHashParams();
  if (params.has('example')) {
    return params.get('example')!;
  }
  return Object.keys(examples)[0];
}

function defaultRunner() {
  let params = getHashParams();
  if (params.has('runner')) {
    return params.get('runner') as 'compiler' | 'interpreter';
  }
  return 'interpreter';
}

function updateHashParam(key: string, value: string) {
  let params = getHashParams();
  params.set(key, value);
  window.location.hash = params.toString();
}

async function updateSourceHashparam(source: string) {
  let params = getHashParams();
  params.set('source', await compressSource(source));
  params.delete('example');
  window.location.hash = params.toString();
}

function updateExampleHashparam(example: string) {
  let params = getHashParams();
  params.set('example', example);
  params.delete('source');
  window.location.hash = params.toString();
}

function updateRunnerHashparam(runner: 'compiler' | 'interpreter') {
  updateHashParam('runner', runner);
}

async function updateDisplayHashparam(display: string) {
  updateHashParam('display', display);
}

const emptyTlang = new Tlang('');

@customElement('tlang-playground')
export class TlangPlayground extends LitElement {
  static override styles = css`
    :host {
      display: contents;
    }

    t-shortcuts {
      display: flex;
      flex-direction: column;
      width: 100%;
      height: 100dvh;
    }

    main {
      overflow: hidden;
      flex: 1;
    }

    .toolbar {
      display: flex;
      gap: 1ch;
    }

    .repo-link {
      margin-left: auto;
      margin-right: 1rem;
    }

    .repo-link a {
      text-decoration: none;
      color: var(--ctp-macchiato-mauve);
    }

    .repo-link a:hover {
      text-decoration: underline;
    }

    .editor-split {
      overflow: hidden;
    }

    .editor-split::part(first),
    .output-split::part(first) {
      display: flex;
    }

    t-codemirror,
    .output-tabs {
      flex: 1;
    }

    .output-tabs {
      max-width: 100%;
      max-height: 100%;
    }

    .output-tabs > t-tab-panel {
      flex: 1;
      display: flex;
      max-width: 100%;
      overflow: hidden;
    }

    .output-code,
    .output-ast {
      width: 100%;
      overflow: auto;
    }

    pre {
      font-family: inherit;
      margin: 0;
    }

    .output-split::part(second) {
      flex: 0 0 min-content;
      max-height: calc(50% - 4px);
    }
  `;

  @mediaQuery('(min-width: 980px)')
  private desktop!: boolean;

  @state()
  selectedExample = defaultExample();

  @state()
  source = examples[this.selectedExample];

  @state()
  consoleMessages: ConsoleMessage[] = [];

  @state()
  display: OutputDisplay = defaultDisplay();

  get availableDisplayOptions(): OutputDisplay[] {
    if (this.runner === 'interpreter') {
      return ['hir_pretty', 'hir', 'ast'];
    } else {
      return ['javascript', 'hir_pretty', 'hir', 'ast'];
    }
  }

  @state()
  showConsole = true;

  /**
   * No need to be a state, as it is derived on `source` change.
   */
  private tlang: Tlang = emptyTlang;

  @state() runner: 'compiler' | 'interpreter' = defaultRunner();

  // The editor which the user can use to write code, as it's always rendered,
  // we cache the query selector.
  @query('.editor', true)
  codemirror!: TCodeMirror;

  @query('.output-split', true)
  outputSplit!: SplitElement;

  @query('t-console', true)
  consoleElement!: ConsoleElement;

  private tlangConsole = {
    log: (...args: unknown[]) => {
      this.logToConsole('log', ...args);
    },
    warn: (...args: unknown[]) => {
      this.logToConsole('warn', ...args);
    },
    error: (...args: unknown[]) => {
      this.logToConsole('error', ...args);
    },
    group: (...args: unknown[]) => {
      this.logToConsole('group', ...args);
    },
    groupEnd: (...args: unknown[]) => {
      this.logToConsole('groupEnd', ...args);
    },
  };

  private analyze() {
    try {
      this.tlang.analyze();

      this.codemirror.diagnostics =
        this.tlang
          .getCodemirrorDiagnostics()
          .map(this.mapCodemirrorDiagnosticToJS) ?? [];

      for (let diagnostic of this.tlang.getDiagnostics()) {
        let method =
          diagnostic.severity === 'Error'
            ? ('error' as const)
            : ('warn' as const);

        let formatted = `${diagnostic.message} at ${diagnostic.span.start.line}:${diagnostic.span.start.column}`;

        this.logToConsole(method, formatted);
      }
    } catch {
      // We only log the first parse error to the console, as it's usually the
      // most relevant one.
      for (let parseError of this.tlang.getParseErrors().slice(0, 1)) {
        let formatted = `${Object.keys(parseError.kind)[0]}: ${parseError.msg} at ${parseError.span.start.line}:${parseError.span.start.column}`;

        this.logToConsole('error', formatted);
      }
    }

    this.flushConsole();
  }

  private getConsoleOpenGroups() {
    return this.consoleMessages.filter((message) => message.type === 'group')
      .length;
  }

  run() {
    if (!this.consoleElement.persist) {
      this.clearConsole();
    }

    this.logToConsole('group');

    let beforeOpenGroups = this.getConsoleOpenGroups();

    try {
      if (this.runner === 'compiler') {
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
    this.flushConsole();
  }

  private logToConsole(method: ConsoleMessage['type'], ...args: unknown[]) {
    this.consoleMessages.push(createConsoleMessage(method, ...args));
  }

  private flushConsole() {
    this.consoleMessages = this.consoleMessages.slice();
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

  async share() {
    await updateSourceHashparam(this.source);

    navigator.clipboard.writeText(String(window.location)).then(() => {
      let flashnotification = document.createElement('div');

      flashnotification.textContent = 'URL copied to clipboard';
      flashnotification.style.position = 'fixed';
      flashnotification.style.left = '50%';
      flashnotification.style.top = '50%';
      flashnotification.style.transform = 'translate(-50%, -50%)';
      flashnotification.style.padding = '1rem';
      flashnotification.style.background = 'var(--ctp-macchiato-surface0)';
      flashnotification.style.borderRadius = '8px';
      flashnotification.style.zIndex = '1000';

      document
        .querySelector('body > t-live[role="log"]')!
        .append(flashnotification);

      setTimeout(() => flashnotification.remove(), 1000);
    });
  }

  private mapCodemirrorDiagnosticToJS(diagnostic: CodemirrorDiagnostic) {
    return {
      from: diagnostic.from,
      to: diagnostic.to,
      message: diagnostic.message,
      severity: diagnostic.severity as CodemirrorSeverity,
    };
  }

  protected createTlang(source: string) {
    this.tlang = new Tlang(source);

    this.tlang.defineFunction('log', this.tlangConsole.log);

    for (let [method, fn] of Object.entries(this.tlangConsole)) {
      this.tlang.defineFunction(`log::${method}`, fn);
    }

    this.analyze();
  }

  protected override update(changedProperties: PropertyValueMap<this>): void {
    super.update(changedProperties);

    if (changedProperties.has('source')) {
      this.createTlang(this.source);
    }
  }

  protected override firstUpdated(
    _changedProperties: PropertyValueMap<this>,
  ): void {
    let params = new URLSearchParams(window.location.hash.slice(1));

    let exampleName = String(params.get('example'));

    if (exampleName in examples) {
      this.shadowRoot!.querySelector('select')!.value = exampleName;
    }

    defaultSource().then((source) => {
      this.codemirror.source = source;
    });
  }

  protected override updated(changedProperties: PropertyValueMap<this>): void {
    if (changedProperties.has('runner')) {
      // TODO: This is a hack to update the select value. Lit doesn't seem to
      //       be super happy about dynamically updated options.
      let select =
        this.shadowRoot?.querySelector<HTMLSelectElement>('.toolbar__display');

      if (select) {
        select.value = this.display;
        updateDisplayHashparam(this.display);
      }
    }
  }

  handleSourceChange(event: CustomEvent) {
    this.source = event.detail.source;
  }

  handleExampleSelect(event: Event) {
    const target = event.target as HTMLSelectElement;
    this.codemirror.source = examples[target.value];
    this.selectedExample = target.value;
    updateExampleHashparam(this.selectedExample);
  }

  handleRunnerChange(event: Event) {
    const target = event.target as HTMLSelectElement;
    this.runner = target.value as 'compiler' | 'interpreter';

    // When using the interpreter, showing the javascript output does not make
    // sense.
    if (this.runner === 'interpreter' && this.display === 'javascript') {
      this.display = 'hir_pretty';
    }

    updateRunnerHashparam(this.runner);
  }

  handleDisplayChange(event: CustomEvent<{ id: string }>) {
    this.display = event.detail.id as OutputDisplay;
    updateDisplayHashparam(this.display);
  }

  handleConsoleCollapse(event: CustomEvent<{ collapsed: boolean }>) {
    this.outputSplit.disabled = event.detail.collapsed ? 'resize-only' : false;

    if (event.detail.collapsed) {
      this.outputSplit.reset();
    } else {
      this.outputSplit.restore();
    }
  }

  handleOutputSplitToggle(event: SplitEvent) {
    if (this.outputSplit.isTouched) {
      // Default behavior, reset the split.
      return;
    }

    event.preventDefault();

    this.outputSplit.restoreOrReset();
    this.consoleElement.collapse();
  }

  handleConsoleClear(event: Event) {
    event.preventDefault();

    this.clearConsole();
  }

  private clearConsole() {
    this.consoleMessages = [];
  }

  showKeyboardShortcuts() {
    this.shadowRoot!.querySelector('t-shortcuts')!.showShortcutsReference();
  }

  renderOutput() {
    switch (this.display) {
      case 'ast':
        return html`<pre class="output-ast">${this.tlang.getASTString()}</pre>`;
      case 'hir':
        return html`<pre class="output-ast">${this.tlang.getHIRString()}</pre>`;
      case 'hir_pretty':
        return html`
          <t-codemirror
            class="output-code"
            language="tlang"
            .source=${this.tlang.getHIRPretty()}
            with-diagnostics="false"
            readonly
          ></t-codemirror>
        `;
      case 'javascript':
        return html`
          <t-codemirror
            class="output-code"
            language="javascript"
            .source=${this.tlang.getJavaScript()}
            with-diagnostics="false"
            readonly
          ></t-codemirror>
        `;
    }
  }

  protected override render() {
    return html`
      <t-shortcuts>
        <header>
          <div class="toolbar">
            <t-button
              @click=${this.run}
              shortcut="ctrl+enter"
              shortcut-description="Run Code"
            >
              Run
            </t-button>
            <select
              class="toolbar__runner"
              @change=${this.handleRunnerChange}
              .value=${live(this.runner)}
            >
              <option value="interpreter">Interpreter</option>
              <option value="compiler">Compiler (JS)</option>
            </select>
            <t-button @click=${this.share}>Share</t-button>
            <select
              class="toolbar__example"
              @change=${this.handleExampleSelect}
              .value=${live(this.selectedExample)}
            >
              ${repeat(
                Object.keys(examples),
                (key) => key,
                (key) => html`<option>${key}</option>`,
              )}
            </select>
            <t-button @click=${this.showKeyboardShortcuts} aria-label="Help">
              ?
            </t-button>
            <div class="repo-link">
              <a href="https://github.com/topaxi/tlang">Source Code</a>
            </div>
          </div>
        </header>
        <main>
          <t-split
            class="editor-split"
            direction=${this.desktop ? 'vertical' : 'horizontal'}
          >
            <t-codemirror
              slot="first"
              class="editor"
              @source-change=${this.handleSourceChange}
            ></t-codemirror>
            <t-split
              slot="second"
              direction="horizontal"
              class="output-split"
              @t-split-toggle=${this.handleOutputSplitToggle}
            >
              <t-tabs
                class="output-tabs"
                slot="first"
                single
                .selected=${this.display}
                @t-tab-select=${this.handleDisplayChange}
              >
                ${repeat(
                  this.availableDisplayOptions,
                  (display) => display,
                  (display) => html`
                    <t-tab slot="tab" id=${display}>
                      ${displayLabels[display]}
                    </t-tab>
                  `,
                )}
                <t-tab-panel>
                  ${keyed(this.display, this.renderOutput())}
                </t-tab-panel>
              </t-tabs>
              <t-console
                slot="second"
                .messages=${this.consoleMessages}
                @collapse=${this.handleConsoleCollapse}
                @clear=${this.handleConsoleClear}
              >
              </t-console>
            </t-split>
          </t-split>
        </main>
      </t-shortcuts>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tlang-playground': TlangPlayground;
  }
}
