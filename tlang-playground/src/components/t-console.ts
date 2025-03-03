import { css, html, LitElement, PropertyValueMap } from 'lit';
import { customElement, property, query, state } from 'lit/decorators.js';
import { repeat } from 'lit/directives/repeat.js';

function stringify(value: unknown) {
  function bigintToJSON(n: bigint) {
    if (n > BigInt(Number.MAX_SAFE_INTEGER)) {
      return n.toString();
    } else if (n < BigInt(Number.MIN_SAFE_INTEGER)) {
      return n.toString();
    } else {
      return Number(n);
    }
  }

  return JSON.stringify(value, (_, v) =>
    typeof v === 'bigint' ? bigintToJSON(v) : v,
  );
}

export interface ConsoleMessage {
  type: 'log' | 'warn' | 'error' | 'group' | 'groupEnd';
  args?: string | unknown[];
  timestamp: number;
}

export function createConsoleMessage(
  type: ConsoleMessage['type'],
  ...args: unknown[]
): ConsoleMessage {
  return { type, args, timestamp: Date.now() };
}

@customElement('t-console')
export class ConsoleElement extends LitElement {
  static styles = css`
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
  collapsed = false;

  @state()
  showTimestamps = false;

  @query('.messages-container')
  consoleMessagesContainer!: HTMLElement;

  collapsedGroups = new WeakSet<ConsoleMessage>();

  collapse(state = !this.collapsed) {
    this.collapsed = state;
  }

  toggleTimestamps() {
    this.showTimestamps = !this.showTimestamps;
  }

  private handleCollapse(_event: Event) {
    this.collapse();

    this.dispatchEvent(
      new CustomEvent('collapse', {
        detail: { collapsed: this.collapsed },
      }),
    );
  }

  private handleConsoleClear(_event: Event) {
    this.dispatchEvent(new CustomEvent('clear'));
  }

  protected updated(changedProperties: PropertyValueMap<this>): void {
    if (changedProperties.has('messages')) {
      this.consoleMessagesContainer.scrollTop =
        this.consoleMessagesContainer.scrollHeight;
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

  protected render() {
    let groupStack = 0;
    let collapsedAt = Number.MAX_SAFE_INTEGER;

    let isCollapsed = () => groupStack >= collapsedAt;
    let resetCollapsedAt = () => (collapsedAt = Number.MAX_SAFE_INTEGER);

    let hasIcons = this.messages.some((message) => message.type === 'error');

    return html`
      <div class="toolbar">
        <div id="title" class="toolbar__title">Console</div>
        <button
          @click=${this.toggleTimestamps}
          aria-pressed=${String(this.showTimestamps)}
        >
          ${this.showTimestamps ? 'Hide Timestamps' : 'Show Timestamps'}
        </button>
        <button
          @click=${this.handleCollapse}
          aria-pressed=${String(this.collapsed)}
          aria-controls="messages"
        >
          ${this.collapsed ? 'Expand Console' : 'Collapse Console'}
        </button>
        <button @click=${this.handleConsoleClear}>Clear</button>
      </div>
      <div class="messages-container">
        <div
          id="messages"
          class="messages"
          .hidden=${this.collapsed}
          aria-expanded=${String(this.collapsed)}
        >
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
        </ul>
      </div>
    `;
  }
}

@customElement('t-console-message')
export class ConsoleMessageElement extends LitElement {
  static styles = css`
    :host {
      display: flex;
      position: relative;
      margin-top: -1px;
      padding-left: 1ch;

      --console-message-color: var(--ctp-macchiato-text);
      --console-message-border-color: var(--ctp-macchiato-surface0);

      color: var(--console-message-color);
      background: var(--console-message-background);
      border-top: 1px solid var(--console-message-border-color);
      border-bottom: 1px solid var(--console-message-border-color);
    }

    :host([type='error']) {
      --console-message-color: var(--ctp-macchiato-red);

      --console-message-border-color: hsl(
        from hsl(var(--ctp-macchiato-red-hsl)) h s calc(l - 40)
      );

      --console-message-background: hsl(
        from hsl(var(--ctp-macchiato-red-hsl)) h s calc(l - 60)
      );
    }

    [part='timestamp'] {
      color: var(--ctp-macchiato-subtext0);
      margin-right: 8px;
    }

    [part='collapse'] {
      all: unset;
      appearance: none;
      margin-right: 8px;
    }

    [part='collapse']::after {
      content: '';
      position: absolute;
      top: 0;
      right: 0;
      bottom: 0;
      left: 0;
    }

    [part='indent'] {
      display: block;
      min-width: 12px;
      max-width: 12px;
      margin-left: 4px;
      border-left: 1px solid var(--ctp-macchiato-sapphire);
      margin-top: -1px;
      margin-bottom: -1px;
    }

    [part^='icon'] {
      user-select: none;
      font-style: normal;
      margin-right: 1ch;
    }

    [part^='icon']::before {
      content: attr(icon, '\\a0');
    }

    [part='args'] {
      display: flex;
      gap: 1ch;
    }

    [part='stack'] {
      display: block;
      padding-left: 2ch;
      white-space: pre-wrap;
    }
  `;

  @property({ type: String })
  type: ConsoleMessage['type'] = 'log';

  @property({ type: Object })
  message!: ConsoleMessage;

  @property({ type: Number })
  timestamp = 0;

  @property({ type: Number })
  indent = 0;

  @property({ type: Boolean })
  collapsed = false;

  @property({ type: Boolean })
  showTimestamp = false;

  @property({ type: Boolean })
  forceIcon = false;

  private collapse() {
    this.dispatchEvent(
      new CustomEvent('collapse', {
        detail: this.message,
      }),
    );
  }

  protected renderMessageIcon() {
    switch (this.type) {
      case 'error':
        return html`<i part="icon icon-${this.type}" icon=""></i>`;
      default:
        return this.forceIcon
          ? html`<i part="icon icon-${this.type} icon-none"></i>`
          : null;
    }
  }

  protected renderConsoleMessageArgs(args: ConsoleMessage['args']) {
    if (args == null || args.length === 0) {
      return;
    }

    if (typeof args === 'string') {
      return args;
    }

    return html`${this.renderMessageIcon()}<span part="args">
        ${args.map(this.renderConsoleMessageArg, this)}
      </span>`;
  }

  protected renderConsoleMessageArg(arg: unknown) {
    switch (typeof arg) {
      case 'string':
        return html`<span part="arg">${arg}</span>`;
      case 'object':
        if (arg == null) {
          return html`<span part="arg arg-null">null</span>`;
        }

        if (arg instanceof Error) {
          return html`
            <span part="arg arg-error">
              ${String(arg)}
              <span part="stack">${arg.stack}</span>
            </span>
          `;
        }
        break;
    }

    return html`<span part="arg">${stringify(arg)}</span>`;
  }

  private renderTimestamp(ts: Date) {
    let hours = ts.getHours().toString().padStart(2, '0');
    let minutes = ts.getMinutes().toString().padStart(2, '0');
    let seconds = ts.getSeconds().toString().padStart(2, '0');
    let milliseconds = ts.getMilliseconds().toString().padStart(3, '0');

    return `${hours}:${minutes}:${seconds}.${milliseconds}`;
  }

  protected render() {
    let rendered = this.renderConsoleMessageArgs(this.message.args);

    if (this.type === 'group' && this.indent > 0) {
      rendered = html`<button
          part="collapse"
          aria-label="Toggle Group"
          @click=${this.collapse}
        >
          ${this.collapsed ? '' : ''}</button
        >${rendered}`;
    }

    for (
      let indent = this.indent - Number(this.type === 'group');
      indent > 0;
      indent--
    ) {
      rendered = html`<span part="indent"></span>${rendered}`;
    }

    if (rendered != null && this.showTimestamp) {
      let date = new Date(this.timestamp);

      rendered = html`
        <time part="timestamp" datetime=${date.toJSON()}>
          ${this.renderTimestamp(date)}
        </time>
        ${rendered}
      `;
    }

    return rendered;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-console': ConsoleElement;
    't-console-message': ConsoleMessageElement;
  }
}
