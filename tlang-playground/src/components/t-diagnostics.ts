import { css, html, LitElement, PropertyValueMap } from 'lit';
import { customElement, property, query, state } from 'lit/decorators.js';
import { unsafeHTML } from 'lit/directives/unsafe-html.js';
import { mediaQuery } from '../decorators/media-query';
import './t-icon';
import './t-message';
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
      font-size: 0.875rem;
    }

    t-message {
      padding: 0.5rem 1ch;
      white-space: pre-wrap;
    }

    t-message:last-child {
      border-bottom: none;
    }

    dialog {
      background-color: var(--t-background-color);
      color: var(--t-foreground-color);
      border: none;
      outline: none;
      padding: 0;
      margin: 0;
      width: 100%;
      height: 100%;
      max-width: 100dvw;
      max-height: 100dvh;
      overflow: hidden;
    }

    dialog[open] {
      display: flex;
      flex-direction: column;
    }

    dialog::backdrop {
      background: rgb(0 0 0 / 50%);
    }

    dialog .messages {
      flex: 1;
    }
  `;

  @mediaQuery('(max-width: 640px), (max-height: 800px)')
  private compact!: boolean;

  @property({ type: Array })
  messages: DiagnosticMessage[] = [];

  @property({ type: Number })
  errorCount = 0;

  @property({ type: Number })
  warningCount = 0;

  @state()
  private expanded = false;

  @query('dialog')
  private dialog!: HTMLDialogElement | null;

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

  @state()
  private wasCompact = false;

  protected override updated(): void {
    // Collapse inline expansion when entering compact mode
    if (this.compact && !this.wasCompact && this.expanded) {
      this.expanded = false;
    }
    this.wasCompact = this.compact;

    if (this.compact) {
      if (this.expanded && !this.dialog?.open) {
        this.dialog?.showModal();
      } else if (!this.expanded && this.dialog?.open) {
        this.dialog?.close();
      }
    } else {
      if (this.dialog?.open) {
        this.dialog?.close();
      }
    }
  }

  private toggle() {
    this.expanded = !this.expanded;
  }

  private onDialogClose() {
    this.expanded = false;
  }

  private renderToolbar() {
    return html`
      <div class="toolbar">
        <t-toggle-button
          type="expandable"
          .pressed=${this.expanded}
          @click=${this.toggle}
          title=${this.expanded ? 'Collapse Diagnostics' : 'Expand Diagnostics'}
          aria-label=${this.expanded
            ? 'Collapse Diagnostics'
            : 'Expand Diagnostics'}
        >
          <t-icon
            name=${this.expanded ? 'chevron-down' : 'chevron-right'}
          ></t-icon>
        </t-toggle-button>
        <span class="toolbar__title">Diagnostics</span>
        ${this.errorCount > 0
          ? html`<span class="severity-count severity-count--error">
              ${this.errorCount} ${this.errorCount === 1 ? 'error' : 'errors'}
            </span>`
          : null}
        ${this.warningCount > 0
          ? html`<span class="severity-count severity-count--warning">
              ${this.warningCount}
              ${this.warningCount === 1 ? 'warning' : 'warnings'}
            </span>`
          : null}
      </div>
    `;
  }

  private renderMessages() {
    return html`
      <div class="messages" role="list" aria-label="Diagnostic messages">
        ${this.messages.map(
          // prettier-ignore
          (msg) => html`<t-message role="listitem" severity=${msg.severity}><div>${unsafeHTML(msg.html)}</div></t-message>`,
        )}
      </div>
    `;
  }

  protected override render() {
    return html`
      ${this.renderToolbar()}
      ${this.compact
        ? html`
            <dialog @close=${this.onDialogClose} aria-label="Diagnostics">
              ${this.expanded
                ? html`${this.renderToolbar()} ${this.renderMessages()}`
                : null}
            </dialog>
          `
        : this.expanded
          ? this.renderMessages()
          : null}
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-diagnostics': DiagnosticsElement;
  }
}
