import { provide } from '@lit/context';
import { LitElement, html, css } from 'lit';
import { customElement, property, query, state } from 'lit/decorators.js';
import {
  type ShortcutsContextValue,
  shortcutsContext,
} from '../contexts/shortcuts';
import { documentListener } from '../decorators/document-listener';
import { hostListener } from '../decorators/host-listener';
import { repeat } from 'lit/directives/repeat.js';
import { isMac } from '../utils/platform-detection';
import {
  formatKeyshortcutsPretty,
  parseMacShortcutDefinition,
  parseShortcutDefinition,
  ShortcutDefinition,
} from '../utils/shortcuts';

const HELP_KEYS = ['?', 'F1'];

function isHelpKey(event: KeyboardEvent): boolean {
  return HELP_KEYS.includes(event.key);
}

export type ShortcutEventName = 't-shortcut-register' | 't-shortcut-unregister';

const SHORTCUT_CATEGORIES = ['global', 'editor', 'preview', 'console'] as const;

export type ShortcutCategory = (typeof SHORTCUT_CATEGORIES)[number];

export interface ShortcutEventDetail {
  shortcut: string;
  shortcutMac?: string;
  shortcutDescription: string;
  shortcutCategory?: ShortcutCategory;
  shortcutPriority?: number;
}

export function pickShortcutEventDetail(
  obj: ShortcutEventDetail,
): ShortcutEventDetail {
  return {
    shortcut: obj.shortcut,
    shortcutMac: obj.shortcutMac,
    shortcutDescription: obj.shortcutDescription,
    shortcutCategory: obj.shortcutCategory,
    shortcutPriority: obj.shortcutPriority,
  };
}

export class ShortcutEvent<
  T extends ShortcutEventDetail = ShortcutEventDetail,
> extends CustomEvent<T> {
  static register<T extends ShortcutEventDetail>(detail: T): ShortcutEvent<T> {
    return new ShortcutEvent('t-shortcut-register', { detail });
  }

  override type!: ShortcutEventName;

  static unregister<T extends ShortcutEventDetail>(
    detail: T,
  ): ShortcutEvent<T> {
    return new ShortcutEvent('t-shortcut-unregister', { detail });
  }

  constructor(type: ShortcutEventName, eventInit: CustomEventInit<T>) {
    super(type, { ...eventInit, composed: true, bubbles: true });
  }
}

const defaultShortcuts = Object.fromEntries(
  SHORTCUT_CATEGORIES.map((category) => [category, []]),
) as unknown as Record<
  ShortcutCategory,
  ReadonlyArray<
    ShortcutEventDetail & {
      shortcut: string;
      shortcutCategory: ShortcutCategory;
      shortcutPriority: number;
    }
  >
>;

defaultShortcuts.global = [
  {
    shortcut: 'F1,?',
    shortcutCategory: 'global',
    shortcutDescription: 'Show Keyboard Shortcuts',
    shortcutPriority: 1000, // Low prio, you somehow managed to open this help
  },
];

@customElement('t-shortcuts')
export class ShortcutsElement extends LitElement {
  static override styles = css`
    dialog {
      background-color: var(--t-background-color);
      border: 1px solid var(--ctp-macchiato-surface0);
      border-radius: var(--t-border-radius);
      outline: none;
      padding: 0;
      box-shadow: 4px 4px 16px rgba(0, 0, 0, 0.33);
    }

    dialog::backdrop {
      background: rgba(0, 0, 0, 0.5);
    }

    header {
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
    }

    header,
    section {
      display: flex;
      padding: 0.25rem 1rem;
    }

    section {
      gap: 2rem;
      flex-wrap: wrap;
    }

    ul {
      list-style: none;
      padding: 0;
      margin: 0.5rem 0;
    }

    ::part(kbd) {
      font-size: inherit;
    }
  `;

  @query('[part="shortcutlegend"]', true)
  private shortcutsElement!: HTMLDialogElement;

  @provide({ context: shortcutsContext })
  @state()
  private shortcutsContextValue: ShortcutsContextValue = { showHints: false };

  @state()
  private shortcuts: Readonly<typeof defaultShortcuts> = defaultShortcuts;

  showShortcutsReference(): void {
    this.shortcutsElement.showModal();
  }

  private setShortcuts(
    updater: (shortcuts: ShortcutsContextValue) => ShortcutsContextValue,
  ): void {
    this.shortcutsContextValue = updater(this.shortcutsContextValue);
  }

  private setShowHints(showHints: boolean): void {
    if (this.shortcutsContextValue.showHints === showHints) {
      return;
    }

    this.setShortcuts((shortcuts) => ({ ...shortcuts, showHints }));
  }

  @hostListener(['t-shortcut-register', 't-shortcut-unregister'])
  @documentListener(['keydown', 'keyup'])
  handleEvent(e: KeyboardEvent | ShortcutEvent): void {
    const event = e as
      | (KeyboardEvent & { type: `key${string}` })
      | ShortcutEvent;

    switch (event.type) {
      case 'keyup': {
        if (isHelpKey(event)) {
          return this.showShortcutsReference();
        }

        this.setShowHints(false);
        break;
      }

      case 'keydown': {
        if (event.altKey) {
          this.setShowHints(true);
        }
        break;
      }

      case 't-shortcut-register': {
        return this.registerShortcut(event.detail);
      }

      case 't-shortcut-unregister': {
        return this.unregisterShortcut(event.detail);
      }
    }
  }

  private registerShortcut({
    shortcut,
    shortcutMac,
    shortcutDescription,
    shortcutCategory = 'global',
    shortcutPriority = 0,
  }: ShortcutEventDetail) {
    if (this.shortcuts[shortcutCategory].some((s) => s.shortcut === shortcut)) {
      this.unregisterShortcut({ shortcut, shortcutCategory });
    }

    this.shortcuts = {
      ...this.shortcuts,
      [shortcutCategory]: [
        ...this.shortcuts[shortcutCategory],
        {
          shortcut,
          shortcutMac,
          shortcutDescription,
          shortcutCategory,
          shortcutPriority,
        } satisfies ShortcutEventDetail,
      ].sort((a, b) => a.shortcutPriority - b.shortcutPriority),
    };
  }

  private unregisterShortcut({
    shortcut,
    shortcutCategory = 'global',
  }: Pick<ShortcutEventDetail, 'shortcut' | 'shortcutCategory'>) {
    if (
      this.shortcuts[shortcutCategory].every((s) => s.shortcut !== shortcut)
    ) {
      return;
    }

    this.shortcuts = {
      ...this.shortcuts,
      [shortcutCategory]: this.shortcuts[shortcutCategory].filter(
        (s) => s.shortcut !== shortcut,
      ),
    };
  }

  protected override render(): unknown {
    return html`
      <slot></slot>
      <dialog part="shortcutlegend" popover>
        <header>
          <h2>Keyboard Shortcuts</h2>
        </header>

        <section>
          ${SHORTCUT_CATEGORIES.map((category) => {
            if (this.shortcuts[category].length === 0) {
              return '';
            }

            return html`
              <div role="group">
                <h3 id=${category}>${category}</h3>
                <ul aria-labelledby=${category}>
                  ${repeat(
                    this.shortcuts[category],
                    (s) => s.shortcut,
                    (s) => html`
                      <li>
                        <t-shortcut-description
                          .shortcutDetail=${s}
                        ></t-shortcut-description>
                      </li>
                    `,
                  )}
                </ul>
              </div>
            `;
          })}
        </section>
      </dialog>
    `;
  }
}

@customElement('t-shortcut-description')
class ShortcutDescriptionElement extends LitElement {
  static override styles = css`
    :host {
      display: flex;
      justify-content: space-between;
      align-items: center;
      min-width: 16rem;
      gap: 1rem;
    }
  `;

  @property({ type: Object })
  shortcutDetail!: ShortcutEventDetail;

  protected override render(): unknown {
    return html`
      <span part="description">${this.shortcutDetail.shortcutDescription}</span>
      <t-shortcut-hint
        part="hint"
        exportparts="kbd"
        shortcut=${this.shortcutDetail.shortcut}
        shortcut-mac=${this.shortcutDetail.shortcutMac}
      ></t-shortcut-hint>
    `;
  }
}

@customElement('t-shortcut-hint')
class ShortcutHintElement extends LitElement {
  static override styles = css`
    [part='kbd'] {
      display: inline-flex;
      align-items: center;
      font-size: 0.75em;
      padding: 0.1em 0.25em;
      border: 1px solid var(--t-button-border-color);
      background-color: var(--t-button-background-color);
    }
  `;

  @property()
  shortcut!: ShortcutDefinition;

  @property({ attribute: 'shortcut-mac' })
  shortcutMac: ShortcutDefinition | '' = '';

  private get shortcutsFormatted() {
    let defs =
      isMac() && this.shortcutMac
        ? this.shortcutMac
            .split(',')
            .map((s) => parseMacShortcutDefinition(s as ShortcutDefinition))
        : this.shortcut
            .split(',')
            .map((s) => parseShortcutDefinition(s as ShortcutDefinition));

    return defs.map(formatKeyshortcutsPretty);
  }

  protected override render(): unknown {
    return this.shortcutsFormatted.map((shortcut, i) => {
      return html`${i > 0 ? ' or ' : ''}<kbd part="kbd">${shortcut}</kbd>`;
    });
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-shortcuts': ShortcutsElement;
    't-shortcut-hint': ShortcutHintElement;
    't-shortcut-description': ShortcutDescriptionElement;
  }
}
