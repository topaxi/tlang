import { css, html, LitElement, PropertyValues, TemplateResult } from 'lit';
import { customElement, property } from 'lit/decorators.js';

@customElement('t-button')
export class ButtonElement extends LitElement {
  static styles = [
    css`
      :host {
        --button-border-color: var(--t-input-border-color);
        --button-focus-ring-color: var(--button-border-color);
        --button-background-color: var(--ctp-macchiato-base);
        --button-hover-background-color: var(--ctp-macchiato-surface1);
        --button-active-background-color: hsl(
          from var(--button-hover-background-color) h s calc(l + 10)
        );

        cursor: default;
        user-select: none;
        border: 1px solid var(--button-border-color);
        border-radius: 3px;
        background-color: var(--button-background-color);
        padding: 0.25em 0.5em;
        font-size: 0.85rem;
      }

      :host(:hover) {
        background-color: var(--button-hover-background-color);
      }

      :host(:active) {
        background-color: var(--button-active-background-color);
      }

      :host(:focus-visible) {
        outline: 2px solid var(--button-focus-ring-color);
      }

      :host([aria-disabled='true']) {
        cursor: not-allowed;
      }
    `,
  ];

  @property({ reflect: true })
  role = 'button';

  @property({ reflect: true })
  tabindex = 0;

  @property({ type: Boolean, attribute: 'aria-disabled', reflect: true })
  disabled = false;

  /**
   * @private
   */
  handleEvent(e: Event): void {
    if (this.disabled) {
      e.preventDefault();
      e.stopImmediatePropagation();
      return;
    }

    let event = e as KeyboardEvent & { type: `key${string}` };

    switch (event.type) {
      case 'keypress':
        if (event.key == 'Enter' || event.key == ' ') {
          this.dispatchEvent(
            new CustomEvent('click', {
              bubbles: true,
              composed: true,
            }),
          );
        }
        break;
    }
  }

  protected firstUpdated(_changedProperties: PropertyValues): void {
    this.addEventListener('keypress', this);
  }

  protected render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-button': ButtonElement;
  }
}
