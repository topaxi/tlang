import { css, html, LitElement, PropertyValueMap } from 'lit';
import { customElement, property, query, state } from 'lit/decorators.js';

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
  return { type, args: args.map(stringify), timestamp: Date.now() };
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

    li:not(:first-child)
      > t-console-message:not([type='groupEnd'][indent='0']) {
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
      padding-left: 1ch;
    }

    t-console-message[type='groupEnd'][indent='0'] {
      margin-bottom: 0.5rem;
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

  collapse(state = !this.collapsed) {
    this.collapsed = state;
  }

  toggleTimestamps() {
    this.showTimestamps = !this.showTimestamps;
  }

  private handleCollapse(_event: Event) {
    this.collapse();
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

  protected render() {
    let indent = 0;

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
        <ul
          id="messages"
          class="messages"
          .hidden=${this.collapsed}
          aria-expanded=${String(this.collapsed)}
        >
          ${this.messages.map((message) => {
            if (message.type === 'group') {
              indent += 1;
            }

            let rendered = html`
              <li>
                <t-console-message
                  type=${message.type}
                  indent=${indent - 1}
                  .args=${message.args}
                  .timestamp=${message.timestamp}
                  .showTimestamp=${this.showTimestamps}
                ></t-console-message>
              </li>
            `;

            if (message.type === 'groupEnd') {
              indent -= 1;
            }

            return rendered;
          })}
        </ul>
      </div>
    `;
  }
}

@customElement('t-console-message')
export class ConsoleMessageElement extends LitElement {
  static styles = css`
    :host {
      display: block;
      color: var(--ctp-macchiato-text);
    }

    time {
      color: var(--ctp-macchiato-subtext0);
    }
  `;

  @property({ type: String })
  type: ConsoleMessage['type'] = 'log';

  @property({ type: Array })
  args: ConsoleMessage['args'] = [];

  @property({ type: Number })
  timestamp = 0;

  @property({ type: Number })
  indent = 0;

  @property({ type: Boolean })
  showTimestamp = false;

  protected renderConsoleMessageArgs(args: ConsoleMessage['args']) {
    if (args == null || args.length === 0) {
      return;
    }

    if (typeof args === 'string') {
      return args;
    }

    return html`${args.map((arg) => html`<span>${arg}</span>`)}`;
  }

  private renderTimestamp(ts: Date) {
    let hours = ts.getHours().toString().padStart(2, '0');
    let minutes = ts.getMinutes().toString().padStart(2, '0');
    let seconds = ts.getSeconds().toString().padStart(2, '0');
    let milliseconds = ts.getMilliseconds().toString().padStart(3, '0');

    return `${hours}:${minutes}:${seconds}.${milliseconds}`;
  }

  protected render() {
    let rendered = this.renderConsoleMessageArgs(this.args);

    if (rendered != null && this.showTimestamp) {
      let date = new Date(this.timestamp);

      rendered = html`
        <time datetime=${date.toJSON()}>${this.renderTimestamp(date)}</time>
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
