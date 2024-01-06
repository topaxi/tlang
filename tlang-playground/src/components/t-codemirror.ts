import { EditorView, basicSetup } from "codemirror"
import { catppuccin } from 'codemirror-theme-catppuccin'
import { LitElement, css } from "lit";
import { customElement, property } from "lit/decorators.js";

@customElement('t-codemirror')
export class TCodeMirror extends LitElement {
  static styles = css`
    :host .cm-scroller {
      font-family: inherit;
    }
  `;

  private view: EditorView | null = null

  @property()
  source = ''

  firstUpdated() {

    let updateListener = EditorView.updateListener.of((v) => {
      this.dispatchEvent(new CustomEvent('source-change', {
        detail: {
          source: v.state.doc.toString(),
        },
      }));
    });

    this.view = new EditorView({
      extensions: [basicSetup, catppuccin('macchiato'), updateListener],
      parent: this.shadowRoot as DocumentFragment,
    })
  }

  updated(changedProperties: Map<string, unknown>) {
    if (this.view && changedProperties.has('source')) {
      this.view.dispatch({
        changes: {
          from: 0,
          to: this.view.state.doc.length,
          insert: this.source,
        },
      })
    }
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-codemirror': TCodeMirror
  }
}
