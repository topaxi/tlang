import { css, html, LitElement, TemplateResult } from 'lit';
import { customElement, property } from 'lit/decorators.js';

@customElement('t-menu')
export class MenuElement extends LitElement {
  static styles = css``;

  @property({ type: String, reflect: true })
  role = 'menu';

  protected render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

@customElement('t-menuitem')
export class MenuItemElement extends LitElement {
  static styles = css``;

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
