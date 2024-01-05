import { LitElement, html } from 'lit'
import { customElement, state } from 'lit/decorators.js'
import { compile_to_js } from 'tlang_bindings_js'

@customElement('tlang-playground')
export class TlangPlayground extends LitElement {
  @state()
  source = 'fn main() { 1 |> log() }'

  get output() {
    return compile_to_js(this.source);
  }

  render() {
    return html`
      <textarea .value=${this.source}></textarea>
      <pre>${this.output}</pre>
    `
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tlang-playground': TlangPlayground
  }
}
