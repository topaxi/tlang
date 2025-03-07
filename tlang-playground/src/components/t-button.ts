import { css, html, LitElement, PropertyValues, TemplateResult } from 'lit';
import { customElement, property } from 'lit/decorators.js';
import {
  eventMatchesShortcut,
  parseShortcutDefinition,
  ShortcutDefinition,
  toAriaKeyshortcuts,
} from '../utils/shortcuts';

@customElement('t-button')
export class ButtonElement extends LitElement {
  static styles = [
    css`
      :host {
        align-items: center;
        justify-content: center;
        cursor: default;
        user-select: none;
        border: 1px solid var(--t-button-border-color);
        border-radius: 3px;
        background-color: var(--t-button-background-color);
        padding: 0.25em 0.5em;
        font-size: 0.85rem;
      }

      :host(:not([hidden])) {
        display: inline-flex;
      }

      :host(:hover) {
        background-color: var(--t-button-hover-background-color);
      }

      :host(:active) {
        background-color: var(--t-button-active-background-color);
      }

      :host(:focus-visible) {
        outline: 2px solid var(--t-button-focus-ring-color);
      }

      :host([aria-disabled='true']) {
        cursor: not-allowed;
      }

      [part='shortcut'] {
        display: none;
        font-size: 0.75em;
        margin-left: 0.5em;
        margin-right: -0.25em;
        padding: 0.1em 0.25em;
        border: 1px solid var(--t-button-border-color);
        background-color: var(--t-button-background-color);
      }

      @media (min-width: 980px) {
        [part='shortcut'] {
          display: inline;
        }
      }
    `,
  ];

  @property({ reflect: true })
  role = 'button';

  @property({ reflect: true })
  tabindex = 0;

  @property({ type: Boolean, attribute: 'aria-disabled', reflect: true })
  disabled = false;

  @property({ type: String, reflect: true })
  popovertarget: string | null = null;

  @property({ type: String })
  shortcut: ShortcutDefinition | '' = '';

  private handleShortcut = (e: KeyboardEvent): void => {
    if (this.disabled || !this.shortcut) {
      return;
    }

    if (eventMatchesShortcut(e, parseShortcutDefinition(this.shortcut))) {
      this.dispatchEvent(
        new CustomEvent('click', {
          bubbles: true,
          composed: true,
        }),
      );
    }
  };

  /**
   * @private
   */
  handleEvent(e: Event): void {
    if (this.disabled) {
      e.preventDefault();
      e.stopImmediatePropagation();
      return;
    }

    let event = e as
      | (MouseEvent & { type: 'click' })
      | (KeyboardEvent & { type: `key${string}` });

    switch (event.type) {
      case 'click':
        if (this.popovertarget) {
          let target = (this.getRootNode() as HTMLElement).querySelector(
            `#${this.popovertarget}[popover]`,
          ) as HTMLElement | null;

          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          target?.togglePopover({ source: this } as any);
        }
        break;
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

  firstUpdated(): void {
    this.addEventListener('click', this);
    this.addEventListener('keypress', this);
    this.ownerDocument.addEventListener('keyup', this.handleShortcut);
  }

  protected updated(changedProperties: PropertyValues): void {
    if (changedProperties.has('shortcut') && this.shortcut) {
      this.setAttribute(
        'aria-keyshortcuts',
        toAriaKeyshortcuts(parseShortcutDefinition(this.shortcut)),
      );
    }
  }

  disconnectedCallback(): void {
    super.disconnectedCallback();
    this.ownerDocument.removeEventListener('keyup', this.handleShortcut);
  }

  protected render(): TemplateResult {
    let shortcut = this.shortcut
      ? html`<span aria-hidden="true" part="shortcut">${this.shortcut}</span>`
      : '';

    return html`<slot></slot>${shortcut}`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-button': ButtonElement;
  }
}
