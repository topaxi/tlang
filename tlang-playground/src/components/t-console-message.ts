import { css, html, LitElement } from 'lit';
import { customElement, property } from 'lit/decorators.js';
import './t-button';
import './t-menu';
import './t-toggle-button';

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

@customElement('t-console-message')
export class ConsoleMessageElement extends LitElement {
  static override styles = css`
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

    :host([type='warn']) {
      --console-message-color: var(--ctp-macchiato-yellow);

      --console-message-border-color: hsl(
        from var(--ctp-macchiato-yellow) h s calc(l - 40)
      );

      --console-message-background: hsl(
        from var(--ctp-macchiato-yellow)) h s calc(l - 60)
      );
    }

    :host([type='error']) {
      --console-message-color: var(--ctp-macchiato-red);

      --console-message-border-color: hsl(
        from var(--ctp-macchiato-red) h s calc(l - 40)
      );

      --console-message-background: hsl(
        from var(--ctp-macchiato-red) h s calc(l - 60)
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
      case 'warn':
        return html`<i part="icon icon-${this.type}" icon=""></i>`;
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

  protected override render() {
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
    't-console-message': ConsoleMessageElement;
  }
}
