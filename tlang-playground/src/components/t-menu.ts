import {
  css,
  CSSResultGroup,
  html,
  LitElement,
  PropertyValues,
  TemplateResult,
} from 'lit';
import {
  customElement,
  property,
  queryAssignedElements,
} from 'lit/decorators.js';
import { hostListener } from '../decorators/host-listener';

@customElement('t-menu')
export class MenuElement extends LitElement {
  static override styles = css`
    :host {
      padding: 0;
      border: 1px solid var(--ctp-macchiato-surface0);
      background-color: var(--ctp-macchiato-base);
      scrollbar-width: thin;
    }
  `;

  @queryAssignedElements({
    selector: 't-menuitem,t-menuitem-radio,t-menuitem-checkbox',
  })
  protected menuItems!: MenuItemElement[];

  @property({ type: String, reflect: true })
  override role = 'menu';

  @hostListener('keydown')
  protected handleEvent(e: Event) {
    const event = e as Event | (KeyboardEvent & { type: `key${string}` });

    switch (event.type) {
      case 'slotchange': {
        this.updateTabIndex(0);
        break;
      }
      case 'keydown': {
        break;
      }
    }
  }

  protected override firstUpdated(_changedProperties: PropertyValues): void {
    this.updateTabIndex(0);
  }

  private updateTabIndex(index: number) {
    for (let [i, menuItem] of this.menuItems.entries()) {
      menuItem.tabIndex = i === index ? 0 : -1;
    }
  }

  protected override render(): TemplateResult {
    return html`<slot @slotchange=${this.handleEvent}></slot>`;
  }
}

@customElement('t-menuitem')
export class MenuItemElement extends LitElement {
  static override styles: CSSResultGroup = css`
    :host {
      display: block;
      padding: 0.25em 0.5em;
      cursor: default;
      user-select: none;
      background-color: var(--t-button-background-color);
    }

    :host(:hover),
    :host(:focus) {
      background-color: var(--t-button-hover-background-color);
    }

    :host(:active) {
      background-color: var(--t-button-active-background-color);
    }
  `;

  protected internals = this.attachInternals();

  override tabIndex = -1;

  @property({ type: String, reflect: true })
  override role = 'menuitem';

  @hostListener('keypress')
  protected handleKeyPress(event: KeyboardEvent) {
    if (event.key === 'Enter' || event.key === ' ') {
      this.dispatchEvent(
        new CustomEvent('click', {
          bubbles: true,
          composed: true,
        }),
      );
    }
  }

  protected override render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

@customElement('t-menuitem-radio')
export class MenuItemRadioElement extends MenuItemElement {
  static override styles = MenuItemElement.styles;

  override role = 'menuitemradio';

  protected override render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

@customElement('t-menuitem-checkbox')
export class MenuItemCheckboxElement extends MenuItemElement {
  static override styles = [
    MenuItemElement.styles,
    css`
      ::part(icon)::before {
        content: '\\00a0';
      }

      :host(:state(checked))::part(icon)::before {
        content: '✓';
      }
    `,
  ];

  override role = 'menuitemcheckbox';

  @property({ type: Boolean, reflect: true })
  get checked(): boolean {
    return this.internals.states.has('checked');
  }
  set checked(value: boolean) {
    if (value) {
      this.internals.states.add('checked');
    } else {
      this.internals.states.delete('checked');
    }
  }

  @hostListener('click')
  handleClick(event: MouseEvent) {
    let change = new CustomEvent('change', {
      cancelable: true,
      composed: true,
      bubbles: true,
      detail: { originalEvent: event },
    });

    if (this.dispatchEvent(change)) {
      this.checked = !this.checked;
    }
  }

  protected override render(): TemplateResult {
    return html`
      <slot name="checkbox" part="checkbox"><i part="icon"></i></slot>
      <slot></slot>
    `;
  }
}

@customElement('t-menuitem-separator')
export class MenuItemSeparatorElement extends LitElement {
  @property({ type: String, reflect: true })
  override role = 'separator';
}

declare global {
  interface HTMLElementTagNameMap {
    't-menu': MenuElement;
    't-menuitem': MenuItemElement;
    't-menuitemradio': MenuItemRadioElement;
    't-menuitemcheckbox': MenuItemCheckboxElement;
  }
}
