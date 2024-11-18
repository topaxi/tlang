import { EditorView, basicSetup } from 'codemirror';
import { EditorState } from '@codemirror/state';
import { Diagnostic, linter, lintGutter } from '@codemirror/lint';
import { catppuccin } from 'codemirror-theme-catppuccin';
import { tlangLanguageSupport } from 'codemirror-lang-tlang';
import { javascript } from '@codemirror/lang-javascript';
import { LitElement, css } from 'lit';
import { customElement, property } from 'lit/decorators.js';

@customElement('t-codemirror')
export class TCodeMirror extends LitElement {
  static styles = css`
    :host {
      display: block;
    }

    :host .cm-scroller {
      font-family: inherit;
    }
  `;

  private view: EditorView | null = null;

  @property()
  source = '';

  @property({ type: Boolean })
  readonly = false;

  @property()
  language = 'tlang';

  @property({ type: Array })
  diagnostics: Diagnostic[] = [];

  firstUpdated() {
    let updateListener = EditorView.updateListener.of((v) => {
      this.dispatchEvent(
        new CustomEvent('source-change', {
          detail: {
            source: v.state.doc.toString(),
          },
        }),
      );
    });

    let tlangLint = linter(() => {
      return this.diagnostics;
    });

    this.view = new EditorView({
      extensions: [
        basicSetup,
        catppuccin('macchiato'),
        this.language === 'javascript' ? javascript() : tlangLanguageSupport(),
        this.readonly ? EditorState.readOnly.of(true) : updateListener,
        ...(this.language === 'tlang' ? [tlangLint, lintGutter()] : []),
      ],
      parent: this.shadowRoot as DocumentFragment,
    });
  }

  updated(changedProperties: Map<string, unknown>) {
    if (this.view && changedProperties.has('source')) {
      this.view.dispatch({
        changes: {
          from: 0,
          to: this.view.state.doc.length,
          insert: this.source,
        },
      });
    }
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-codemirror': TCodeMirror;
  }
}
