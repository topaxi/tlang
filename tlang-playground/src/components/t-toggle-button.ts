import { css, PropertyValues } from 'lit';
import { customElement, property } from 'lit/decorators.js';
import { ButtonElement } from './t-button';

@customElement('t-toggle-button')
export class ToggleButtonElement extends ButtonElement {
  static styles = [
    ...ButtonElement.styles,
    css`
      :host([aria-pressed='true']),
      :host([type='expandable'][aria-expanded='true']),
      :host([type='collapsable'][aria-expanded='false']) {
        background-color: var(--button-active-background-color);
      }
    `,
  ];

  /**
   * What type of toggle button is this?
   * - `pressable`: A button that can be toggled on and off.
   * - `expandable`: A button that expands a controlled element.
   * - `collapsable`: A button that collapses a controlled element.
   */
  @property({ reflect: true })
  type: 'pressable' | 'expandable' | 'collapsable' = 'pressable';

  @property()
  controls = '';

  @property({ type: Boolean })
  pressed = false;

  private get host(): Element {
    let rootNode = this.getRootNode();

    if (rootNode instanceof ShadowRoot) {
      return rootNode.host;
    }

    return rootNode as Element;
  }

  private get controlled() {
    if (!this.controls) {
      return null;
    }

    let controlled = this.host.querySelector(this.controls);

    if (controlled?.checkVisibility()) {
      return controlled;
    }

    return null;
  }

  private updatePressed() {
    let ariaAttr = this.type === 'pressable' ? 'aria-pressed' : 'aria-expanded';
    let ariaValue = String(
      this.type === 'collapsable' ? !this.pressed : this.pressed,
    );

    this.setAttribute(ariaAttr, ariaValue);
  }

  protected updated(changedProperties: PropertyValues<this>): void {
    if (changedProperties.has('pressed')) {
      this.updatePressed();
      this.setAttribute('aria-controls', this.controlled?.id ?? '');
    }
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-toggle-button': ToggleButtonElement;
  }
}
