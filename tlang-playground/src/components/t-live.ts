import { html, LitElement, PropertyValues, TemplateResult } from 'lit';
import { customElement, property } from 'lit/decorators.js';

export type AriaLiveRole = 'alert' | 'log' | 'status' | 'timer';
export type AriaLive = 'off' | 'polite' | 'assertive';

@customElement('t-live')
export class LiveElement extends LitElement {
  @property({ type: String, reflect: true })
  override role: string | null = null;

  @property({ attribute: 'aria-live', reflect: true })
  live: AriaLive | null = null;

  protected override firstUpdated(
    _changedProperties: PropertyValues<this>,
  ): void {
    this.live = this.live || this.role == 'alert' ? 'assertive' : 'polite';
  }

  protected override render(): TemplateResult {
    return html`<slot></slot>`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-live': LiveElement;
  }
}
