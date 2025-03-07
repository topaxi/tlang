import { EditorView, basicSetup } from 'codemirror';
import { EditorState } from '@codemirror/state';
import { Diagnostic, linter, lintGutter } from '@codemirror/lint';
import { catppuccin } from 'codemirror-theme-catppuccin';
import { tlangLanguageSupport } from 'codemirror-lang-tlang';
import { javascript } from '@codemirror/lang-javascript';
import { LitElement, css } from 'lit';
import { customElement, property } from 'lit/decorators.js';

export type Language = 'tlang' | 'javascript';

@customElement('t-codemirror')
export class TCodeMirror extends LitElement {
  static styles = css`
    :host {
      display: block;
      min-height: 24em;
    }

    .cm-editor {
      height: 100%;
    }

    /* Added :host to increase specificity */
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
  language: Language = 'tlang';

  @property({
    attribute: 'with-diagnostics',
    type: Boolean,
    converter: (value) => value != null && value !== 'false',
  })
  withDiagnostics = true;

  @property({ type: Array })
  diagnostics: Diagnostic[] = [];

  private dispatchSourceChangeEvent(source: string) {
    this.dispatchEvent(
      new CustomEvent('source-change', { detail: { source } }),
    );
  }

  private getEditorExtensions() {
    let extensions = [basicSetup, catppuccin('macchiato')];

    switch (this.language) {
      case 'tlang':
        extensions.push(tlangLanguageSupport());
        break;

      case 'javascript':
        extensions.push(javascript());
        break;
    }

    if (this.readonly) {
      extensions.push(EditorState.readOnly.of(true));
    } else {
      extensions.push(
        EditorView.updateListener.of((v) => {
          this.dispatchSourceChangeEvent(v.state.doc.toString());
        }),
      );
    }

    if (this.withDiagnostics) {
      extensions.push(linter(() => this.diagnostics));
      extensions.push(lintGutter());
    }

    return extensions;
  }

  private getEditorViewConfig() {
    return {
      extensions: this.getEditorExtensions(),
      parent: this.shadowRoot as DocumentFragment,
    };
  }

  firstUpdated() {
    this.view = new EditorView(this.getEditorViewConfig());
    setTimeout(() => {
      // TODO: Figure out why the scroll position is reset to 0 on first update.
      this.view!.scrollDOM.scrollTop = 0;
    });
  }

  updated(changedProperties: Map<string, unknown>) {
    if (this.view) {
      if (changedProperties.has('source')) {
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

  disconnectedCallback(): void {
    this.view?.destroy();
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-codemirror': TCodeMirror;
  }
}
