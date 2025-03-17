import { css, PropertyValues } from 'lit';
import { customElement, property } from 'lit/decorators.js';
import { ButtonElement } from './t-button';
import { styles } from '../utils/css';

@customElement('t-toggle-button')
export class ToggleButtonElement extends ButtonElement {
  static override styles = styles(
    super.styles,
    css`
      :host([aria-pressed='true']),
      :host([type='expandable'][aria-expanded='true']),
      :host([type='collapsable'][aria-expanded='false']) {
        background-color: var(--t-button-active-background-color);
      }
    `,
  );

  /**
   * What type of toggle button is this?
   * - `pressable`: A button that can be toggled on and off.
   * - `expandable`: A button that expands a controlled element.
   * - `collapsable`: A button that collapses a controlled element.
   */
  @property({ reflect: true })
  type: 'pressable' | 'expandable' | 'collapsable' = 'pressable';

  @property()
  get controls(): string | null {
    return this.getAttribute('aria-controls');
  }
  set controls(value: string | null) {
    this.setAttribute('aria-controls', value ?? '');
  }

  @property({ type: Boolean })
  pressed = false;

  private updatePressed() {
    let ariaAttr = this.type === 'pressable' ? 'aria-pressed' : 'aria-expanded';
    let ariaValue = String(
      this.type === 'collapsable' ? !this.pressed : this.pressed,
    );

    this.setAttribute(ariaAttr, ariaValue);
  }

  protected override updated(changedProperties: PropertyValues<this>): void {
    super.updated(changedProperties);

    if (changedProperties.has('pressed')) {
      this.updatePressed();
    }
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-toggle-button': ToggleButtonElement;
  }
}
