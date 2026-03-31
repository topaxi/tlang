import { LitElement, css, html } from 'lit';
import { customElement, property } from 'lit/decorators.js';
import { type IconName, icon } from '../utils/icons';

@customElement('t-icon')
export class IconElement extends LitElement {
  static override styles = css`
    :host {
      display: inline;
    }
  `;

  @property()
  name!: IconName;

  protected override render() {
    return html`${icon(this.name)}`;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-icon': IconElement;
  }
}
