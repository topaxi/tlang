import { LitElement, PropertyValueMap, css, html } from 'lit';
import { customElement, query, state } from 'lit/decorators.js';
import { examples } from './examples';

import './components/t-codemirror';
import './components/t-split';
import { type TCodeMirror } from './components/t-codemirror';
import { compressSource, decompressSource } from './utils/lz';
import {
  compile,
  getStandardLibraryCompiled,
  TlangCompiler,
  TlangInterpreter,
  CodemirrorDiagnostic,
} from './tlang';
import { MediaController } from './controllers/media-controller';
import { keyed } from 'lit/directives/keyed.js';
import { live } from 'lit/directives/live.js';
import { cache } from 'lit/directives/cache.js';

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

function defaultDisplay() {
  let params = getHashParams();
  if (params.has('display')) {
    return params.get('display') as 'ast' | 'hir' | 'hir pretty' | 'output';
  }
  return 'output';
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

@customElement('tlang-playground')
export class TlangPlayground extends LitElement {
  static styles = css`
    :host {
      display: flex;
      flex-direction: column;
      width: 100%;
      height: 100vh;
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

    .editor,
    .output {
      display: flex;
    }

    .editor > *,
    .output > * {
      width: 100%;
      overflow: auto;
    }

    pre {
      font-family: inherit;
      margin: 0;
    }

    .log-message {
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
      padding-left: 1ch;
    }

    .output-error {
      min-height: 5em;
    }

    .console {
      display: flex;
      flex-direction: column;
      border-left: 1px solid var(--ctp-macchiato-surface0);
      flex: 1 0 25px;
    }

    .console-toolbar {
      display: flex;
      gap: 1rem;
      padding-left: 1ch;
      padding-top: 1rem;
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
    }

    .console-output {
      overflow: auto;
      scroll-behavior: smooth;
    }
  `;

  private desktop = new MediaController(this, '(min-width: 980px)');

  @state()
  selectedExample = defaultExample();

  @state()
  source = examples[this.selectedExample];

  @state()
  consoleOutput: Array<unknown[] | string> = [];

  @state()
  display: 'ast' | 'hir' | 'hir pretty' | 'output' = defaultDisplay();

  @state()
  showConsole = true;

  @query('t-codemirror')
  codemirror!: TCodeMirror;

  @query('.console-output')
  consoleOutputElement!: HTMLElement;

  @state() compiler: TlangCompiler | null = null;

  @state() output = '';

  @state() runner: 'compiler' | 'interpreter' = defaultRunner();

  get error() {
    if (this.compiler == null) {
      return '';
    }

    let { diagnostics, parseErrors } = this.compiler;

    let diagnosticsErrors = diagnostics.filter((diagnostic) =>
      diagnostic.startsWith('ERROR:'),
    );

    if (parseErrors.length + diagnosticsErrors.length) {
      return [...parseErrors, ...diagnostics].join('\n');
    } else {
      return diagnostics.join('\n');
    }
  }

  private compile(source: string) {
    this.compiler = compile(source);
    this.output = this.compiler.output;
  }

  run() {
    if (this.consoleOutput.length > 0) {
      this.consoleOutput.push('---');
    }

    if (this.runner === 'compiler') {
      this.runCompiled();
    } else {
      this.runInterpreted();
    }
  }

  private runCompiled() {
    let fn = new Function(
      'console',
      `${getStandardLibraryCompiled()}\n{${this.output}};`,
    );

    fn({
      log: (...args: unknown[]) => {
        this.consoleOutput = [...this.consoleOutput, args];
      },
    });
  }

  private runInterpreted() {
    let interpreter = new TlangInterpreter();

    interpreter.define_js_fn('log', (...args: unknown[]) => {
      this.consoleOutput = [...this.consoleOutput, args];
    });

    interpreter.eval(this.source);
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

      document.body.append(flashnotification);

      setTimeout(() => flashnotification.remove(), 1000);
    });
  }

  private mapAndFreeCodemirrorDiagnosticToJS(diagnostic: CodemirrorDiagnostic) {
    return {
      from: diagnostic.from,
      to: diagnostic.to,
      message: diagnostic.message,
      severity: diagnostic.severity as CodemirrorSeverity,
    };
  }

  protected update(changedProperties: PropertyValueMap<this>): void {
    super.update(changedProperties);

    if (changedProperties.has('source')) {
      try {
        this.compile(this.source);

        this.codemirror.diagnostics =
          this.compiler?.codemirrorDiagnostics.map(
            this.mapAndFreeCodemirrorDiagnosticToJS,
          ) ?? [];
      } catch (error) {
        console.error(error);
      }
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
      this.run();
    });
  }

  protected updated(changedProperties: PropertyValueMap<this>): void {
    if (changedProperties.has('consoleOutput')) {
      this.consoleOutputElement.scrollTop =
        this.consoleOutputElement.scrollHeight;
    }
  }

  handleSourceChange(event: CustomEvent) {
    this.source = event.detail.source;
    this.consoleOutput = [];
  }

  handleExampleSelect(event: Event) {
    const target = event.target as HTMLSelectElement;
    this.consoleOutput = [];
    this.codemirror.source = examples[target.value];
    this.selectedExample = target.value;
    updateExampleHashparam(this.selectedExample);
    this.run();
  }

  handleRunnerChange(event: Event) {
    const target = event.target as HTMLSelectElement;
    this.runner = target.value as 'compiler' | 'interpreter';
    updateRunnerHashparam(this.runner);
  }

  handleDisplayChange(event: Event) {
    const target = event.target as HTMLSelectElement;
    this.display = target.value as 'ast' | 'hir' | 'hir pretty' | 'output';
    updateDisplayHashparam(this.display);
  }

  private stringify(value: unknown) {
    function bigintToJSON(n: bigint) {
      if (n > BigInt(Number.MAX_SAFE_INTEGER)) {
        return n.toString();
      } else if (n < BigInt(Number.MIN_SAFE_INTEGER)) {
        return n.toString();
      } else {
        return Number(n);
      }
    }
    return JSON.stringify(value, (_, v) =>
      typeof v === 'bigint' ? bigintToJSON(v) : v,
    );
  }

  renderLogMessage(args: string | unknown[]) {
    return html`
      <div class="log-message">
        ${typeof args === 'string' ? args : args.map(this.stringify).join(', ')}
      </div>
    `;
  }

  renderOutput() {
    switch (this.display) {
      case 'ast':
        return html`<pre class="output-ast">${this.compiler?.ast_string}</pre>`;
      case 'hir':
        return html`<pre class="output-ast">${this.compiler?.hir_string}</pre>`;
      case 'hir pretty':
        return html`<t-codemirror
          class="output-code"
          language="tlang"
          .source=${this.compiler?.hir_pretty}
          readonly
        ></t-codemirror>`;
      case 'output':
        return html`<t-codemirror
          class="output-code"
          language="javascript"
          .source=${this.output}
          readonly
        ></t-codemirror>`;
    }
  }

  render() {
    return html`
      <div class="toolbar">
        <button @click=${this.run}>Run</button>
        <select @change=${this.handleRunnerChange} .value=${live(this.runner)}>
          <option value="compiler">Compiler</option>
          <option value="interpreter">Interpreter</option>
        </select>
        <button @click=${this.share}>Share</button>
        <select
          @change=${this.handleExampleSelect}
          .value=${live(this.selectedExample)}
        >
          ${Object.keys(examples).map((key) =>
            keyed(key, html`<option>${key}</option>`),
          )}
        </select>
        <select
          @change=${this.handleDisplayChange}
          .value=${live(this.display)}
        >
          <option value="output">code</option>
          <option value="ast">ast</option>
          <option value="hir">hir</option>
          <option value="hir pretty">hir pretty</option>
        </select>
        <div class="repo-link">
          <a href="https://github.com/topaxi/tlang">Source Code</a>
        </div>
      </div>
      <t-split
        class="editor-split"
        direction=${this.desktop.matches ? 'vertical' : 'horizontal'}
      >
        <div slot="first" class="editor">
          <t-codemirror
            @source-change=${this.handleSourceChange}
          ></t-codemirror>
        </div>
        <t-split slot="second" direction="horizontal" class="output">
          <div slot="first">${cache(this.renderOutput())}</div>
          <div slot="second" class="console">
            <div class="console-toolbar">
              <div>Console</div>
              <button @click=${() => (this.showConsole = !this.showConsole)}>
                ${this.showConsole ? 'Hide' : 'Show'}
              </button>
              <button @click=${() => (this.consoleOutput = [])}>Clear</button>
            </div>
            <div class="console-output" .hidden=${!this.showConsole}>
              ${this.consoleOutput.map((args) => this.renderLogMessage(args))}
            </div>
          </div>
        </t-split>
      </t-split>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tlang-playground': TlangPlayground;
  }
}
