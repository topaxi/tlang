import { css, html, LitElement, PropertyValueMap } from 'lit';
import { customElement, property, query, state } from 'lit/decorators.js';
import { repeat } from 'lit/directives/repeat.js';
import './t-button';
import './t-console-message';
import './t-menu';
import './t-toggle-button';
import { ConsoleMessage } from './t-console-message';
import { floating } from '../directives/floating';
import { MenuItemCheckboxElement } from './t-menu';

export { type ConsoleMessage };

export function createConsoleMessage(
  type: ConsoleMessage['type'],
  ...args: unknown[]
): ConsoleMessage {
  return { type, args, timestamp: Date.now() };
}

@customElement('t-console')
export class ConsoleElement extends LitElement {
  static override styles = css`
    :host {
      display: flex;
      flex-direction: column;
      border-left: 1px solid var(--ctp-macchiato-surface0);
      max-height: 100%;
      overflow: hidden;
    }

    .toolbar {
      display: flex;
      gap: 0.5rem;

      padding: 0.5rem 1ch;
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
    }

    .toolbar__title {
      margin-right: auto;
      align-self: center;
    }

    .messages-container {
      overflow: auto;
      scroll-behavior: smooth;
    }

    .messages {
      margin: 0;
      padding: 0;
      list-style: none;
      min-height: 24em;
    }

    t-console-message[type='group'][indent='0'] {
      border: none;
    }

    t-console-message[type='group'][indent='0']:not(:first-child) {
      margin-top: 0.5rem;
    }
  `;

  @property({ type: Array })
  messages: Array<ConsoleMessage> = [];

  @state()
  private collapsed = false;

  @state()
  private showTimestamps = false;

  @state()
  private _persist = true;

  get persist(): boolean {
    return this._persist;
  }

  @query('.messages-container')
  consoleMessagesContainer!: HTMLElement;

  collapsedGroups = new WeakSet<ConsoleMessage>();

  collapse(state = !this.collapsed) {
    this.collapsed = state;
  }

  toggleTimestamps(event: Event) {
    event.preventDefault();

    this.showTimestamps = !(event.target as MenuItemCheckboxElement).checked;
  }

  togglePersist(event: Event) {
    event.preventDefault();

    this._persist = !(event.target as MenuItemCheckboxElement).checked;
  }

  private handleCollapse(_event: Event) {
    let event = new CustomEvent('collapse', {
      cancelable: true,
      detail: { collapsed: this.collapsed },
    });

    if (this.dispatchEvent(event)) {
      this.collapse();
    }
  }

  private handleConsoleClear(_event: Event) {
    let event = new CustomEvent('clear', { cancelable: true });

    if (this.dispatchEvent(event)) {
      this.messages = [];
    }
  }

  protected override updated(changedProperties: PropertyValueMap<this>): void {
    if (changedProperties.has('messages')) {
      setTimeout(() =>
        this.shadowRoot
          ?.querySelector('t-console-message:last-child')
          ?.scrollIntoView({ block: 'nearest' }),
      );
    }
  }

  private collapseMessage(event: CustomEvent<ConsoleMessage>) {
    let message = event.detail;

    if (this.collapsedGroups.has(message)) {
      this.collapsedGroups.delete(message);
    } else {
      this.collapsedGroups.add(message);
    }

    this.requestUpdate();
  }

  protected override render() {
    let groupStack = 0;
    let collapsedAt = Number.MAX_SAFE_INTEGER;

    let isCollapsed = () => groupStack >= collapsedAt;
    let resetCollapsedAt = () => (collapsedAt = Number.MAX_SAFE_INTEGER);

    let hasIcons = this.messages.some((message) => message.type === 'error');

    return html`
      <div class="toolbar">
        <t-toggle-button
          @click=${this.handleCollapse}
          type="collapsable"
          .pressed=${this.collapsed}
          controls="messages"
          shortcut="ctrl+alt+c"
          shortcut-mac="cmd+alt+c"
          shortcut-category="console"
          shortcut-description="Toggle Console"
          title=${this.collapsed ? 'Expand Console' : 'Collapse Console'}
          aria-label=${this.collapsed ? 'Expand Console' : 'Collapse Console'}
        >
          ${this.collapsed ? '' : ''}
        </t-toggle-button>
        <div id="title" class="toolbar__title">Console</div>
        <t-button
          @click=${this.handleConsoleClear}
          ?hidden=${this.collapsed}
          shortcut="ctrl+l"
          shortcut-mac="cmd+k"
          shortcut-category="console"
          title="Clear Console"
          aria-label="Clear Console"
        >
          
        </t-button>
        <t-button
          popovertarget="menu"
          title="Console Settings"
          aria-label="Console Settings"
        >
          
        </t-button>
        <t-menu id="menu" popover=${floating()}>
          <t-menuitem-checkbox
            @change=${this.togglePersist}
            ?checked=${this._persist}
          >
            Persist Logs
          </t-menuitem-checkbox>
          <t-menuitem-checkbox
            @change=${this.toggleTimestamps}
            ?checked=${this.showTimestamps}
          >
            Show Timestamps
          </t-menuitem-checkbox>
        </t-menu>
      </div>
      <div class="messages-container">
        <div id="messages" class="messages" .hidden=${this.collapsed}>
          ${repeat(
            this.messages,
            (_item, index) => index,
            (message) => {
              if (message.type === 'group') {
                groupStack++;

                if (!isCollapsed() && this.collapsedGroups.has(message)) {
                  collapsedAt = groupStack;
                }
              } else if (message.type === 'groupEnd') {
                groupStack--;

                if (!isCollapsed()) {
                  resetCollapsedAt();
                }

                return null;
              }

              if (
                isCollapsed() &&
                !(message.type === 'group' && collapsedAt === groupStack)
              ) {
                return null;
              }

              let rendered = html`
                <t-console-message
                  type=${message.type}
                  indent=${groupStack - 1}
                  .message=${message}
                  .timestamp=${message.timestamp}
                  .showTimestamp=${this.showTimestamps}
                  .collapsed=${collapsedAt === groupStack}
                  .forceIcon=${hasIcons}
                  @collapse=${this.collapseMessage}
                ></t-console-message>
              `;

              return rendered;
            },
          )}
        </div>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-console': ConsoleElement;
  }
}
