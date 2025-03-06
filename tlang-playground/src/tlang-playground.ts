import { LitElement, PropertyValueMap, css, html } from 'lit';
import { customElement, query, state } from 'lit/decorators.js';
import { examples } from './examples';

import './components/t-button';
import './components/t-codemirror';
import './components/t-console';
import './components/t-live';
import './components/t-split';
import './components/t-tabs';
import { type TCodeMirror } from './components/t-codemirror';
import { ConsoleMessage, createConsoleMessage } from './components/t-console';
import { SplitElement } from './components/t-split';
import { compressSource, decompressSource } from './utils/lz';
import {
  getStandardLibraryCompiled,
  Tlang,
  CodemirrorDiagnostic,
} from './tlang';
import { MediaController } from './controllers/media-controller';
import { keyed } from 'lit/directives/keyed.js';
import { live } from 'lit/directives/live.js';
import { repeat } from 'lit/directives/repeat.js';

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
  let display: OutputDisplay = 'javascript';

  if (params.has('display')) {
    display = params.get('display') as OutputDisplay;
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
  return 'compiler';
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

const emptyTlang = {} as Tlang;

@customElement('tlang-playground')
export class TlangPlayground extends LitElement {
  static styles = css`
    :host {
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

    t-codemirror {
      flex: 1;
    }

    .output-tabs > t-tab-panel {
      display: flex;
      flex: 1;
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

  private desktop = new MediaController(this, '(min-width: 980px)');

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

  @state() tlang: Tlang = emptyTlang;

  @state() runner: 'compiler' | 'interpreter' = defaultRunner();

  // The editor which the user can use to write code, as it's always rendered,
  // we cache the query selector.
  @query('.editor', true)
  codemirror!: TCodeMirror;

  @query('.output-split', true)
  outputSplit!: SplitElement;

  private tlangConsole = {
    log: (...args: unknown[]) => {
      this.logToConsole('log', ...args);
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
        this.tlang.codemirrorDiagnostics.map(
          this.mapCodemirrorDiagnosticToJS,
        ) ?? [];

      for (let diagnostic of this.tlang.diagnostics) {
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
      for (let parseError of this.tlang.parseErrors.slice(0, 1)) {
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
      `${bindingsDestructuring}${getStandardLibraryCompiled()}\n{${this.tlang.js}};`,
    );

    fn(bindings);
  }

  private runInterpreted() {
    this.tlang.define_js_fn('log', this.tlangConsole.log);

    for (let [method, fn] of Object.entries(this.tlangConsole)) {
      this.tlang.define_js_fn(`log::${method}`, fn);
    }

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
    this.analyze();
    this.tlang.compileToJS();
  }

  protected update(changedProperties: PropertyValueMap<this>): void {
    super.update(changedProperties);

    if (changedProperties.has('source')) {
      this.createTlang(this.source);
    }
  }

  protected firstUpdated(_changedProperties: PropertyValueMap<this>): void {
    let params = new URLSearchParams(window.location.hash.slice(1));

    let exampleName = String(params.get('example'));

    if (exampleName in examples) {
      this.shadowRoot!.querySelector('select')!.value = exampleName;
    }

    defaultSource().then((source) => {
      this.codemirror.source = source;
    });
  }

  protected updated(changedProperties: PropertyValueMap<this>): void {
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

  handleDisplayChange(event: Event) {
    this.display = (event as CustomEvent<{ id: string }>).detail
      .id as OutputDisplay;
    updateDisplayHashparam(this.display);
  }

  renderOutput() {
    switch (this.display) {
      case 'ast':
        return html`<pre class="output-ast">${this.tlang.ast_string}</pre>`;
      case 'hir':
        return html`<pre class="output-ast">${this.tlang.hir_string}</pre>`;
      case 'hir_pretty':
        return html`<t-codemirror
          class="output-code"
          language="tlang"
          .source=${this.tlang.hir_pretty}
          with-diagnostics="false"
          readonly
        ></t-codemirror>`;
      case 'javascript':
        return html`<t-codemirror
          class="output-code"
          language="javascript"
          .source=${this.tlang.js}
          with-diagnostics="false"
          readonly
        ></t-codemirror>`;
    }
  }

  render() {
    return html`
      <header>
        <div class="toolbar">
          <t-button @click=${this.run} shortcut="ctrl+alt+r">Run</t-button>
          <select
            class="toolbar__runner"
            @change=${this.handleRunnerChange}
            .value=${live(this.runner)}
          >
            <option value="compiler">Compiler</option>
            <option value="interpreter">Interpreter</option>
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
          <div class="repo-link">
            <a href="https://github.com/topaxi/tlang">Source Code</a>
          </div>
        </div>
      </header>
      <main>
        <t-split
          class="editor-split"
          direction=${this.desktop.matches ? 'vertical' : 'horizontal'}
        >
          <t-codemirror
            slot="first"
            class="editor"
            @source-change=${this.handleSourceChange}
          ></t-codemirror>
          <t-split slot="second" direction="horizontal" class="output-split">
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
                (display) =>
                  html`<t-tab slot="tab" id=${display}>${displayLabels[display]}</option>`,
              )}
              <t-tab-panel>
                ${keyed(this.display, this.renderOutput())}
              </t-tab-panel>
            </t-tabs>
            <t-console
              slot="second"
              .messages=${this.consoleMessages}
              @collapse=${(event: CustomEvent<{ collapsed: boolean }>) => {
                if (event.detail.collapsed) {
                  this.outputSplit.reset();
                }
              }}
              @clear=${() => (this.consoleMessages = [])}
            >
            </t-console>
          </t-split>
        </t-split>
      </main>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tlang-playground': TlangPlayground;
  }
}
