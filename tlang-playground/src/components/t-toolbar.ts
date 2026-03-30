import { css, html, LitElement } from 'lit';
import { customElement } from 'lit/decorators.js';

@customElement('t-toolbar')
export class TlangToolbar extends LitElement {
  static override styles = css`
    :host,
    slot[name='end'] {
      display: flex;
      flex-wrap: wrap;
      gap: 1ch;

      /* By default, no wrapping text, components which want to allow wrapping
       * can do so explicitly. */
      white-space: nowrap;
    }

    slot[name='end'] {
      margin-left: auto;
    }
  `;

  protected override render(): unknown {
    return html`
      <slot></slot>
      <slot part="end" name="end"></slot>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-toolbar': TlangToolbar;
  }
}
