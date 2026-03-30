import { css, html, LitElement } from 'lit';
import { customElement, property } from 'lit/decorators.js';

export type MessageSeverity = 'error' | 'warning' | 'log';

@customElement('t-message')
export class TMessageElement extends LitElement {
  static override styles = css`
    :host {
      display: flex;
      position: relative;
      margin-top: -1px;

      --message-color: var(--ctp-macchiato-text);
      --message-border-color: var(--ctp-macchiato-surface0);

      color: var(--message-color);
      background: var(--message-background);
      border-top: 1px solid var(--message-border-color);
      border-bottom: 1px solid var(--message-border-color);
    }

    :host([severity='warning']) {
      --message-color: var(--ctp-macchiato-yellow);
      --message-border-color: hsl(
        from var(--ctp-macchiato-yellow) h s calc(l - 40)
      );
      --message-background: hsl(
        from var(--ctp-macchiato-yellow) h s calc(l - 60)
      );
    }

    :host([severity='error']) {
      --message-color: var(--ctp-macchiato-red);
      --message-border-color: hsl(
        from var(--ctp-macchiato-red) h s calc(l - 40)
      );
      --message-background: hsl(from var(--ctp-macchiato-red) h s calc(l - 60));
    }
  `;

  @property({ type: String, reflect: true })
  severity: MessageSeverity = 'log';

  protected override render() {
    return html`<slot></slot>`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-message': TMessageElement;
  }
}
