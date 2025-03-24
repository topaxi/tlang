import { basicSetup } from 'codemirror';
import { EditorView, keymap } from '@codemirror/view';
import { EditorState, Prec } from '@codemirror/state';
import { Diagnostic, linter, lintGutter } from '@codemirror/lint';
import { catppuccin } from 'codemirror-theme-catppuccin';
import { tlangLanguageSupport } from 'codemirror-lang-tlang';
import { javascript } from '@codemirror/lang-javascript';
import { LitElement, css } from 'lit';
import { customElement, property } from 'lit/decorators.js';
import { hostListener } from '../decorators/host-listener';

export type Language = 'tlang' | 'javascript';

@customElement('t-codemirror')
export class TCodeMirror extends LitElement {
  static override styles = css`
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

  @property({ type: Boolean, attribute: 'with-diagnostics' })
  withDiagnostics = false;

  @property({ type: Array })
  diagnostics: Diagnostic[] = [];

  @hostListener('keyup')
  handleKeyUp(event: KeyboardEvent) {
    // ? will show the shortcuts popover, but within the editor we do not
    // want that but instead want to insert a question mark.
    if (event.key === '?') {
      event.stopPropagation();
    }
  }

  private dispatchSourceChangeEvent(source: string) {
    this.dispatchEvent(
      new CustomEvent('source-change', { detail: { source } }),
    );
  }

  private getEditorExtensions() {
    let extensions = [
      basicSetup,
      catppuccin('macchiato'),
      // We currently use Ctrl+Enter to run the code.
      Prec.highest(keymap.of([{ key: 'Ctrl-Enter', run: () => true }])),
    ];

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

  protected override firstUpdated() {
    this.view = new EditorView(this.getEditorViewConfig());
    setTimeout(() => {
      // TODO: Figure out why the scroll position is reset to 0 on first update.
      this.view!.scrollDOM.scrollTop = 0;
    });
  }

  protected override updated(changedProperties: Map<string, unknown>) {
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

  override disconnectedCallback(): void {
    this.view?.destroy();
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-codemirror': TCodeMirror;
  }
}
