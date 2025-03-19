import { css, html, LitElement, PropertyValues, TemplateResult } from 'lit';
import { customElement, property } from 'lit/decorators.js';

export interface FlashElementOptions {
  severity?: 'info' | 'error';
  global?: boolean;
}

@customElement('t-flash')
export class FlashElement extends LitElement {
  static override styles = css`
    :host {
      position: fixed;
      padding: 1rem;
      background-color: var(--t-background-color);
      border: 1px solid var(--ctp-macchiato-surface0);
      border-radius: var(--t-border-radius);
      box-shadow: 4px 4px 16px rgba(0, 0, 0, 0.33);
      margin: 0;
    }

    :host([position='center']) {
      left: 50%;
      top: 50%;
      transform: translate(-50%, -50%);
    }
  `;

  @property({ type: String, reflect: true })
  position = 'center';

  @property({ type: Number, attribute: 'auto-dismiss', reflect: true })
  autoDismiss = 0;

  @property({ type: String, reflect: true })
  severity: 'info' | 'error' = 'info';

  @property({ type: Boolean, reflect: true })
  global = true;

  override popover = 'manual';

  constructor(options: FlashElementOptions = {}) {
    super();

    this.severity = options.severity || this.severity;
    this.global = options.global || this.global;

    if (this.global) {
      this.appendToGlobalContainer();
    }
  }

  private appendToGlobalContainer(): void {
    switch (this.severity) {
      case 'info':
        this.ownerDocument
          .querySelector('body > t-live[role="log"]')
          ?.append(this);
        break;
      case 'error':
        this.ownerDocument
          .querySelector('body > t-live[role="alert"]')
          ?.append(this);
    }
  }

  protected override firstUpdated(_changedProperties: PropertyValues): void {
    this.showPopover();

    if (this.autoDismiss > 0) {
      setTimeout(() => this.remove(), this.autoDismiss);
    }
  }

  protected override render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-flash': FlashElement;
  }
}
