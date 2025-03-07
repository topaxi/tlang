import { css, html, LitElement, TemplateResult } from 'lit';
import { customElement, property } from 'lit/decorators.js';

@customElement('t-menu')
export class MenuElement extends LitElement {
  static styles = css`
    :host {
      padding: 0;
      border: 1px solid var(--ctp-macchiato-surface0);
      background-color: var(--ctp-macchiato-base);
      scrollbar-width: thin;
    }
  `;

  @property({ type: String, reflect: true })
  role = 'menu';

  protected render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

@customElement('t-menuitem')
export class MenuItemElement extends LitElement {
  static styles = css`
    :host {
      display: block;
      padding: 0.25em 0.5em;
      cursor: default;
      user-select: none;
      background-color: var(--t-button-background-color);
    }

    :host(:hover) {
      background-color: var(--t-button-hover-background-color);
    }

    :host(:active) {
      background-color: var(--t-button-active-background-color);
    }
  `;

  @property({ type: String, reflect: true })
  role = 'menuitem';

  protected render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

@customElement('t-menuitem-radio')
export class MenuItemRadioElement extends MenuItemElement {
  static styles = MenuItemElement.styles;

  @property({ type: String, reflect: true })
  role = 'menuitemradio';

  protected render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

@customElement('t-menuitem-checkbox')
export class MenuItemCheckboxElement extends MenuItemElement {
  static styles = MenuItemElement.styles;

  @property({ type: String, reflect: true })
  role = 'menuitemradio';

  protected render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

@customElement('t-menuitem-separator')
export class MenuItemSeparatorElement extends LitElement {
  @property({ type: String, reflect: true })
  role = 'separator';
}

declare global {
  interface HTMLElementTagNameMap {
    't-menu': MenuElement;
    't-menuitem': MenuItemElement;
    't-menuitemradio': MenuItemRadioElement;
    't-menuitemcheckbox': MenuItemCheckboxElement;
  }
}
