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
    }

    .message {
      appearance: none;
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
      padding-left: 1ch;
    }
  `;

  @property({ type: Array })
  messages: Array<string | unknown[]> = [];

  @state()
  collapsed = false;

  @query('.messages-container')
  consoleMessagesContainer!: HTMLElement;

  collapse(state = !this.collapsed) {
    this.collapsed = state;
  }

  private renderConsoleMessage(args: string | unknown[]) {
    return html`
      <li class="message">
        ${typeof args === 'string' ? args : args.map(stringify).join(', ')}
      </li>
    `;
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
    return html`
      <div class="toolbar">
        <div class="toolbar__title">Console</div>
        <button @click=${() => this.collapse()}>
          ${this.collapsed ? 'Show' : 'Hide'}
        </button>
        <button @click=${this.handleConsoleClear}>Clear</button>
      </div>
      <div class="messages-container">
        <ul class="messages" .hidden=${this.collapsed}>
          ${this.messages.map(this.renderConsoleMessage, this)}
        </ul>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-console': ConsoleElement;
  }
}
