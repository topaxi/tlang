import { css, html, LitElement, PropertyValues, TemplateResult } from 'lit';
import { customElement, property } from 'lit/decorators.js';
import {
  eventMatchesShortcut,
  parseShortcutDefinition,
  ShortcutDefinition,
  toAriaKeyshortcuts,
} from '../utils/shortcuts';
import { documentListener } from '../decorators/document-listener';
import { hostListener } from '../decorators/host-listener';

@customElement('t-button')
export class ButtonElement extends LitElement {
  static override styles = [
    css`
      :host {
        --button-border-color: var(--t-input-border-color);
        --button-focus-ring-color: var(--button-border-color);
        --button-background-color: var(--ctp-macchiato-base);
        --button-hover-background-color: var(--ctp-macchiato-surface1);
        --button-active-background-color: hsl(
          from var(--button-hover-background-color) h s calc(l + 10)
        );

        align-items: center;
        justify-content: center;
        cursor: default;
        user-select: none;
        border: 1px solid var(--button-border-color);
        border-radius: 3px;
        background-color: var(--button-background-color);
        padding: 0.25em 0.5em;
        font-size: 0.85rem;
      }

      :host(:not([hidden])) {
        display: inline-flex;
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

      [part='shortcut'] {
        display: none;
        font-size: 0.75em;
        margin-left: 0.5em;
        margin-right: -0.25em;
        padding: 0.1em 0.25em;
        border: 1px solid var(--button-border-color);
        background-color: var(--button-background-color);
      }

      @media (min-width: 980px) {
        [part='shortcut'] {
          display: inline;
        }
      }
    `,
  ];

  @property({ type: String, reflect: true })
  override role = 'button';

  @property({ reflect: true })
  tabindex = 0;

  @property({ type: Boolean, attribute: 'aria-disabled', reflect: true })
  disabled = false;

  @property({ type: String })
  shortcut: ShortcutDefinition | '' = '';

  @documentListener('keyup')
  protected handleShortcut(e: KeyboardEvent) {
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
  }

  @hostListener('keypress')
  protected handleEvent(e: Event): void {
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

  protected override updated(changedProperties: PropertyValues<this>): void {
    if (changedProperties.has('shortcut') && this.shortcut) {
      this.setAttribute(
        'aria-keyshortcuts',
        toAriaKeyshortcuts(parseShortcutDefinition(this.shortcut)),
      );
    }
  }

  override disconnectedCallback(): void {
    super.disconnectedCallback();
  }

  protected override render(): TemplateResult {
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
