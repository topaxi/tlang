import { css, html, LitElement, PropertyValueMap } from 'lit';
import { customElement, property, state } from 'lit/decorators.js';
import { unsafeHTML } from 'lit/directives/unsafe-html.js';
import './t-toggle-button';

export interface DiagnosticMessage {
  severity: 'error' | 'warning';
  html: string;
}

@customElement('t-diagnostics')
export class DiagnosticsElement extends LitElement {
  static override styles = css`
    :host {
      display: flex;
      flex-direction: column;
      border-top: 1px solid var(--ctp-macchiato-surface0);
      max-height: 40%;
      overflow: hidden;
    }

    :host([hidden]) {
      display: none;
    }

    .toolbar {
      display: flex;
      align-items: center;
      gap: 0.5rem;
      padding: 0.25rem 1ch;
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
      flex-shrink: 0;
    }

    .toolbar__title {
      margin-right: auto;
      align-self: center;
      font-size: 0.875rem;
    }

    .severity-count {
      display: inline-flex;
      align-items: center;
      gap: 0.25em;
      font-size: 0.875rem;
    }

    .severity-count--error {
      color: var(--ctp-macchiato-red);
    }

    .severity-count--warning {
      color: var(--ctp-macchiato-yellow);
    }

    .messages {
      overflow: auto;
    }

    .message {
      padding: 0.5rem 1ch;
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
      font-size: 0.875rem;
      white-space: pre-wrap;
    }

    .message:last-child {
      border-bottom: none;
    }

    .message--error {
      --console-message-color: var(--ctp-macchiato-red);
      --console-message-border-color: hsl(
        from var(--ctp-macchiato-red) h s calc(l - 40)
      );
      --console-message-background: hsl(
        from var(--ctp-macchiato-red) h s calc(l - 60)
      );

      color: var(--console-message-color);
      background: var(--console-message-background);
      border-color: var(--console-message-border-color);
    }

    .message--warning {
      --console-message-color: var(--ctp-macchiato-yellow);
      --console-message-border-color: hsl(
        from var(--ctp-macchiato-yellow) h s calc(l - 40)
      );
      --console-message-background: hsl(
        from var(--ctp-macchiato-yellow) h s calc(l - 60)
      );

      color: var(--console-message-color);
      background: var(--console-message-background);
      border-color: var(--console-message-border-color);
    }
  `;

  @property({ type: Array })
  messages: DiagnosticMessage[] = [];

  @property({ type: Number })
  errorCount = 0;

  @property({ type: Number })
  warningCount = 0;

  @state()
  private expanded = false;

  protected override willUpdate(
    changedProperties: PropertyValueMap<this>,
  ): void {
    if (changedProperties.has('messages')) {
      this.hidden = this.messages.length === 0;

      if (this.messages.length === 0) {
        this.expanded = false;
      }
    }
  }

  private toggle() {
    this.expanded = !this.expanded;
  }

  protected override render() {
    return html`
      <div class="toolbar">
        <t-toggle-button
          type="collapsable"
          .pressed=${!this.expanded}
          @click=${this.toggle}
          title=${this.expanded ? 'Collapse Diagnostics' : 'Expand Diagnostics'}
          aria-label=${this.expanded
            ? 'Collapse Diagnostics'
            : 'Expand Diagnostics'}
        >
          ${this.expanded ? '' : ''}
        </t-toggle-button>
        <span class="toolbar__title">Diagnostics</span>
        ${this.errorCount > 0
          ? html`<span
              class="severity-count severity-count--error"
              aria-label="${this.errorCount} ${this.errorCount === 1
                ? 'error'
                : 'errors'}"
            >
              ${this.errorCount} ${this.errorCount === 1 ? 'error' : 'errors'}
            </span>`
          : null}
        ${this.warningCount > 0
          ? html`<span
              class="severity-count severity-count--warning"
              aria-label="${this.warningCount} ${this.warningCount === 1
                ? 'warning'
                : 'warnings'}"
            >
              ${this.warningCount}
              ${this.warningCount === 1 ? 'warning' : 'warnings'}
            </span>`
          : null}
      </div>
      ${this.expanded
        ? html`
            <div class="messages" aria-label="Diagnostic messages">
              ${this.messages.map(
                (msg) => html`
                  <div class="message message--${msg.severity}">
                    ${unsafeHTML(msg.html)}
                  </div>
                `,
              )}
            </div>
          `
        : null}
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-diagnostics': DiagnosticsElement;
  }
}
