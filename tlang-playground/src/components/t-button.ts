import { css, html, LitElement, PropertyValues, TemplateResult } from 'lit';
import { customElement, property } from 'lit/decorators.js';
import {
  eventMatchesMacShortcutDefinition,
  eventMatchesShortcutDefinition,
  parseMacShortcutDefinition,
  parseShortcutDefinition,
  ShortcutDefinition,
  toAriaKeyshortcuts,
} from '../utils/shortcuts';
import { documentListener } from '../decorators/document-listener';
import { hostListener } from '../decorators/host-listener';
import { consume } from '@lit/context';
import { shortcutsContext, ShortcutsContextValue } from '../contexts/shortcuts';
import { isMac } from '../utils/platform-detection';
import './t-shortcuts';
import {
  pickShortcutEventDetail,
  ShortcutCategory,
  ShortcutEvent,
} from './t-shortcuts';

@customElement('t-button')
export class ButtonElement extends LitElement {
  static override styles = [
    css`
      :host {
        align-items: center;
        justify-content: center;
        cursor: default;
        user-select: none;
        border: 1px solid var(--t-button-border-color);
        border-radius: var(--t-border-radius);
        background-color: var(--t-button-background-color);
        padding: 0.25em 0.5em;
        font-size: 0.85rem;
        min-width: 1.5em;
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
      }

      @media (min-width: 980px) {
        [part='shortcut'] {
          display: inline;
        }
      }

      ::part(kbd) {
        margin-left: 0.5em;
        margin-right: -0.25em;
      }
    `,
  ];

  @property({ type: String, reflect: true })
  override role = 'button';

  override tabIndex = 0;

  @property({ type: Boolean, attribute: 'aria-disabled', reflect: true })
  disabled = false;

  @property({ type: String, reflect: true })
  popovertarget: string | null = null;

  @property({ type: String })
  shortcut: ShortcutDefinition | '' = '';

  @property({ type: String, attribute: 'shortcut-mac' })
  shortcutMac: ShortcutDefinition | '' = '';

  @property({ type: String, attribute: 'shortcut-description' })
  shortcutDescription: string = '';

  @property({ type: String, attribute: 'shortcut-category' })
  shortcutCategory: ShortcutCategory = 'global';

  @property({ type: Number, attribute: 'shortcut-priority' })
  shortcutPriority = 0;

  @consume({ context: shortcutsContext, subscribe: true })
  private shortcutsContext!: ShortcutsContextValue;

  private popoverOpen = false;

  @documentListener('keyup')
  protected handleShortcut(e: KeyboardEvent) {
    if (this.disabled || !this.shortcut) {
      return;
    }

    if (
      eventMatchesShortcutDefinition(e, this.shortcut) ||
      (this.shortcutMac &&
        eventMatchesMacShortcutDefinition(e, this.shortcutMac))
    ) {
      this.dispatchEvent(
        new CustomEvent('click', {
          bubbles: true,
          composed: true,
        }),
      );
    }
  }

  private getPopoverTarget(): HTMLElement | null {
    if (!this.popovertarget) {
      return null;
    }

    let target = (this.getRootNode() as HTMLElement).querySelector(
      `#${this.popovertarget}[popover]`,
    ) as HTMLElement | null;

    if (target == null) {
      throw new Error(
        `No popover target found with id "${this.popovertarget}"`,
      );
    }

    return target;
  }

  private _togglePopover(force?: boolean): void {
    let target = this.getPopoverTarget();

    if (target == null) {
      return;
    }

    if (!this.popoverOpen) {
      target.addEventListener(
        'toggle',
        (event) => {
          this.popoverOpen = (event as ToggleEvent).newState === 'open';

          target.addEventListener(
            'toggle',
            (event) => {
              this.popoverOpen = (event as ToggleEvent).newState === 'open';
            },
            { once: true },
          );
        },
        { once: true },
      );
    }

    target.togglePopover(force);
  }

  @hostListener(['click', 'keypress'])
  protected handleEvent(e: Event): void {
    if (this.disabled) {
      e.preventDefault();
      e.stopImmediatePropagation();
      return;
    }

    let event = e as
      | (MouseEvent & { type: 'click' })
      | (KeyboardEvent & { type: `key${string}` });

    switch (event.type) {
      case 'click': {
        let target = this.getPopoverTarget();

        if (target == null || (target.popover === 'auto' && this.popoverOpen)) {
          // Clicking outside the popover will close it already,
          // so we don't need to do anything
          return;
        }

        this._togglePopover();
        break;
      }
      case 'keypress': {
        if (event.key == 'Enter' || event.key == ' ') {
          let syntheticClick = new CustomEvent('click', {
            cancelable: true,
            bubbles: true,
            composed: true,
          });

          if (this.dispatchEvent(syntheticClick)) {
            this._togglePopover(!this.popoverOpen);
          }
        }
        break;
      }
    }
  }

  protected override updated(changedProperties: PropertyValues<this>): void {
    if (changedProperties.has('shortcut')) {
      if (!this.shortcut) {
        this.removeAttribute('aria-keyshortcuts');
        this.dispatchEvent(ShortcutEvent.unregister(this));
      } else {
        this.setAttribute(
          'aria-keyshortcuts',
          toAriaKeyshortcuts(
            isMac() && this.shortcutMac
              ? parseMacShortcutDefinition(this.shortcutMac)
              : parseShortcutDefinition(this.shortcut),
          ),
        );

        let shortcutDescription =
          this.shortcutDescription ||
          this.ariaLabel ||
          this.title ||
          this.textContent ||
          '';

        this.dispatchEvent(
          ShortcutEvent.register({
            ...pickShortcutEventDetail(this),
            shortcutDescription,
          }),
        );
      }
    }
  }

  override disconnectedCallback(): void {
    super.disconnectedCallback();

    if (this.shortcut) {
      this.dispatchEvent(ShortcutEvent.unregister(this));
    }
  }

  protected override render(): TemplateResult {
    let shortcut =
      this.shortcut && this.shortcutsContext.showHints
        ? html`
            <t-shortcut-hint
              aria-hidden="true"
              part="shortcut"
              exportparts="kbd"
              .shortcut=${this.shortcut}
              .shortcutMac=${this.shortcutMac}
            ></t-shortcut-hint>
          `
        : '';

    return html`
      <slot aria-hidden=${Boolean(this.ariaLabel)}></slot>${shortcut}
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-button': ButtonElement;
  }
}
