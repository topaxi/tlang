import { EditorView, basicSetup } from 'codemirror';
import { EditorState } from '@codemirror/state';
import { linter, Diagnostic, setDiagnostics } from "@codemirror/lint"
import { catppuccin } from 'codemirror-theme-catppuccin';
import { tlangLanguageSupport } from 'codemirror-lang-tlang';
import { javascript } from '@codemirror/lang-javascript';
import { LitElement, css } from 'lit';
import { customElement, property } from 'lit/decorators.js';

@customElement('t-codemirror')
export class TCodeMirror extends LitElement {
  static styles = css`
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
  diagnostics: unknown[] = [];

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
      let diagnostics = this.diagnostics
        .map((d: any) => ({
          message: d.message,
          severity: d.severity,
          from: d.from,
          to: d.to,
        }))
        // TODO: Use of undeclared variable spans are not correct and span the whole block.
        .filter((d: Diagnostic) => d.message.startsWith('Use of undeclared'));

      return diagnostics;
    });

    this.view = new EditorView({
      extensions: [
        basicSetup,
        catppuccin('macchiato'),
        this.language === 'javascript' ? javascript() : tlangLanguageSupport(),
        this.readonly ? EditorState.readOnly.of(true) : updateListener,
        ...(this.language === 'tlang' ? [tlangLint] : []),
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
