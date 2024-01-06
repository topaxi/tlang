import { LitElement, PropertyValueMap, css, html } from 'lit'
import { customElement, query, state } from 'lit/decorators.js'
import init, { compile_to_js, parse_and_analyze, parse_to_ast } from 'tlang_bindings_js'
import { examples } from './examples';

import './components/t-codemirror';
import { TCodeMirror } from './components/t-codemirror';

await init();

@customElement('tlang-playground')
export class TlangPlayground extends LitElement {
  static styles = css`
    :host {
      display: flex;
      flex-direction: column;
      width: 100%;
      min-height: 100vh;
    }

    .editor {
      display: flex;
      flex: 1 1 auto;
      max-height: 100%;
    }

    .editor > * {
      flex: 1 0 50%;
      overflow: auto;
      max-height: 100%;
    }

    pre {
      margin: 0;
    }

    .log-message {
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
    }

    .output-console {
      border-top: 1px solid var(--ctp-macchiato-surface0);
    }
  `

  @state()
  source = examples['factorial.tl'];

  @state()
  consoleOutput: unknown[][] = [];

  @state()
  display: 'ast' | 'semanticAST' | 'output' = 'output';

  @query('t-codemirror')
  codemirror!: TCodeMirror;

  get output() {
    try {
      return compile_to_js(this.source);
    } catch (error: any) {
      return error.message;
    }
  }

  get ast() {
    try {
      return parse_to_ast(this.source);
    }
    catch (error: any) {
      return error.message;
    }
  }

  get semanticAST() {
    try {
      return parse_and_analyze(this.source);
    } catch (error: any) {
      return error.message;
    }
  }

  run() {
    this.consoleOutput = [];

    let fn = new Function('console', this.output);

    fn({
      log: (...args: unknown[]) => {
        this.consoleOutput = [...this.consoleOutput, args]
      }
    });
  }

  protected firstUpdated(_changedProperties: PropertyValueMap<any> | Map<PropertyKey, unknown>): void {
    this.codemirror.source = this.source;
  }

  handleSourceChange(event: CustomEvent) {
    this.source = event.detail.source;
    this.consoleOutput = [];
  }

  handleSelect(event: Event) {
    const target = event.target as HTMLSelectElement
    this.consoleOutput = [];
    this.codemirror.source = examples[target.value];
  }

  renderLogMessage(args: unknown[]) {
    return html`<div class="log-message">${args.map(arg => JSON.stringify(arg)).join(' ')}</div>`
  }

  render() {
    return html`
      <div class="toolbar">
        <button @click=${this.run}>Run</button>
        <select @change=${this.handleSelect}>
          ${Object.keys(examples).map(key => html`<option>${key}</option>`)}
        </select>
        <select @change=${(event: Event) => this.display = (event.target as HTMLSelectElement).value as typeof this.display}>
          <option value="output">code</option>
          <option value="ast">ast</option>
          <option value="semanticAST">semantic ast</option>
        </select>
      </div>
      <div class="editor">
        <t-codemirror @source-change=${this.handleSourceChange}></t-codemirror>
        <div class="output">
          ${this.display === 'ast' ? html`<pre class="output-ast">${this.ast}</pre>` : ''}
          ${this.display === 'semanticAST' ? html`<pre class="output-ast">${this.semanticAST}</pre>` : ''}
          ${this.display === 'output' ? html`<pre class="output-code">${this.output}</pre>` : ''}
          <div class="console">
            <div class="console-toolbar">
              <button @click=${() => this.consoleOutput = []}>Clear</button>
            </div>
            <pre class="output-console">${this.consoleOutput.map(args => this.renderLogMessage(args))}</pre>
          </div>
        </div>
      </div>
    `
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tlang-playground': TlangPlayground
  }
}
