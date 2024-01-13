import { LitElement, PropertyValueMap, css, html } from 'lit';
import { customElement, query, state } from 'lit/decorators.js';
import LZString from 'lz-string';
import init, {
  compile_to_js,
  parse_and_analyze,
  parse_to_ast,
} from 'tlang_bindings_js';
import { examples } from './examples';

import './components/t-codemirror';
import { TCodeMirror } from './components/t-codemirror';

await init();

// Default source code is either the code provided via hashcode "source"
// compressed by lz-string or the "example" hashcode with the corresponding
// example file name. Default is the first example.
function defaultSource() {
  let params = new URLSearchParams(window.location.hash.slice(1));

  if (params.has('source')) {
    return LZString.decompressFromEncodedURIComponent(params.get('source')!);
  } else if (params.has('example')) {
    let exampleName = params.get('example') ?? '';
    if (exampleName in examples) {
      return examples[exampleName];
    }
  }
  return examples[Object.keys(examples)[0]];
}

function updateSourceHashparam(source: string) {
  window.location.hash = `source=${LZString.compressToEncodedURIComponent(
    source,
  )}`;
}

@customElement('tlang-playground')
export class TlangPlayground extends LitElement {
  static styles = css`
    :host {
      display: grid;
      width: 100%;
      height: 100vh;

      grid-template:
        'toolbar' auto
        'editor' 1fr
        'output' 1fr
        'console' auto;
    }

    @media (min-width: 1000px) {
      :host {
        grid-template:
          'toolbar toolbar' auto
          'editor output' 1fr
          'editor console' auto;
      }
    }

    .toolbar {
      display: flex;
      gap: 1ch;
      grid-area: toolbar;
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

    .editor {
      display: flex;
      flex: 1 1 auto;
      max-height: 100%;

      grid-area: editor;
    }

    .editor > * {
      flex: 1 0 50%;
      overflow: auto;
      max-height: 100%;
    }

    pre {
      font-family: inherit;
      margin: 0;
    }

    .log-message {
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
      padding-left: 1ch;
    }

    .output {
      display: flex;
      flex-direction: column;
      grid-area: output;
      max-height: 100%;
      overflow: hidden;
    }

    @media (min-width: 760px) {
      .output {
        padding-left: 1ch;
        border-left: 1px solid var(--ctp-macchiato-surface0);
      }
    }

    .output > * {
      overflow: auto;
    }

    .console {
      border-left: 1px solid var(--ctp-macchiato-surface0);
      padding-top: 1rem;
      grid-area: console;
    }

    .console-toolbar {
      display: flex;
      gap: 1rem;
      padding-left: 1ch;
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
    }

    .console-output {
      height: 24em;
      overflow: auto;
      scroll-behavior: smooth;
    }
  `;

  @state()
  selectedExample = Object.keys(examples)[0];

  @state()
  source = examples[this.selectedExample];

  @state()
  consoleOutput: Array<unknown[] | string> = [];

  @state()
  display: 'ast' | 'semanticAST' | 'output' = 'output';

  @state()
  showConsole = true;

  @query('t-codemirror')
  codemirror!: TCodeMirror;

  @query('.console-output')
  consoleOutputElement!: HTMLElement;

  @state() output = '';

  @state() ast = '';

  @state() semanticAST = '';

  @state() error = '';

  run() {
    if (this.consoleOutput.length > 0) {
      this.consoleOutput.push('---');
    }

    let fn = new Function('console', this.output);

    fn({
      log: (...args: unknown[]) => {
        this.consoleOutput = [...this.consoleOutput, args];
      },
    });
  }

  share() {
    updateSourceHashparam(this.source);

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

  protected update(
    changedProperties: PropertyValueMap<any> | Map<PropertyKey, unknown>,
  ): void {
    super.update(changedProperties);

    if (changedProperties.has('source')) {
      try {
        this.error = '';
        let ast = parse_to_ast(this.source);
        this.ast = JSON.stringify(ast, null, 2);
        let semanticAST = parse_and_analyze(this.source);
        this.semanticAST = JSON.stringify(semanticAST, null, 2);
        let js = compile_to_js(this.source);
        this.output = js;
      } catch (error) {
        this.error = String(error);
      }
    }
  }

  protected firstUpdated(
    _changedProperties: PropertyValueMap<any> | Map<PropertyKey, unknown>,
  ): void {
    let params = new URLSearchParams(window.location.hash.slice(1));

    let exampleName = String(params.get('example'));

    if (exampleName in examples) {
      this.shadowRoot!.querySelector('select')!.value = exampleName;
    }

    this.codemirror.source = defaultSource();
  }

  protected updated(
    changedProperties: PropertyValueMap<any> | Map<PropertyKey, unknown>,
  ): void {
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
    window.location.hash = `example=${encodeURIComponent(target.value)}`;
  }

  renderLogMessage(args: string | unknown[]) {
    return html`
      <div class="log-message">
        ${typeof args === 'string'
          ? args
          : args.map((arg) => JSON.stringify(arg)).join(', ')}
      </div>
    `;
  }

  render() {
    return html`
      <div class="toolbar">
        <button @click=${this.run}>Run</button>
        <button @click=${this.share}>Share</button>
        <select @change=${this.handleExampleSelect}>
          ${Object.keys(examples).map((key) => html`<option>${key}</option>`)}
        </select>
        <select
          @change=${(event: Event) =>
            (this.display = (event.target as HTMLSelectElement)
              .value as typeof this.display)}
        >
          <option value="output">code</option>
          <option value="ast">ast</option>
          <option value="semanticAST">semantic ast</option>
        </select>
        <div class="repo-link">
          <a href="https://github.com/topaxi/tlang">Source Code</a>
        </div>
      </div>
      <div class="editor">
        <t-codemirror @source-change=${this.handleSourceChange}></t-codemirror>
      </div>
      <div class="output">
        ${this.display === 'ast'
          ? html`<pre class="output-ast">${this.ast}</pre>`
          : ''}
        ${this.display === 'semanticAST'
          ? html`<pre class="output-ast">${this.semanticAST}</pre>`
          : ''}
        ${this.display === 'output'
          ? html`<t-codemirror
              class="output-code"
              language="javascript"
              .source=${this.output}
              readonly
            ></t-codemirror>`
          : ''}
        ${this.error ? html`<pre class="output-error">${this.error}</pre>` : ''}
      </div>
      <div class="console">
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
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tlang-playground': TlangPlayground;
  }
}
