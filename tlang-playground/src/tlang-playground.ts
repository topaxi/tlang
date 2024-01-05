import { LitElement, css, html } from 'lit'
import { customElement, state } from 'lit/decorators.js'
import init, { compile_to_js, parse_and_analyze, parse_to_ast } from 'tlang_bindings_js'
import { examples } from './examples';

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
  `

  @state()
  source = examples['factorial.tl'];

  @state()
  consoleOutput: unknown[][] = [];

  @state()
  display: 'ast' | 'semanticAST' | 'output' = 'output';

  get output() {
    return compile_to_js(this.source);
  }

  get ast() {
    return parse_to_ast(this.source);
  }

  get semanticAST() {
    return parse_and_analyze(this.source);
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

  handleInput(event: Event) {
    const target = event.target as HTMLTextAreaElement
    this.source = target.value
    this.consoleOutput = [];
  }

  handleSelect(event: Event) {
    const target = event.target as HTMLSelectElement
    this.source = examples[target.value]
    this.consoleOutput = [];
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
        <textarea spellcheck="false" .value=${this.source} @input=${this.handleInput}></textarea>
        <div class="output">
          ${this.display === 'ast' ? html`<pre class="output-ast">${this.ast}</pre>` : ''}
          ${this.display === 'semanticAST' ? html`<pre class="output-ast">${this.semanticAST}</pre>` : ''}
          ${this.display === 'output' ? html`<pre class="output-code">${this.output}</pre>` : ''}
          <pre class="output-console">${this.consoleOutput.map(args => this.renderLogMessage(args))}</pre>
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
