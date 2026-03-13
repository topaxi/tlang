import { css, html, LitElement, PropertyValues, TemplateResult } from 'lit';
import {
  customElement,
  property,
  query,
  queryAssignedElements,
} from 'lit/decorators.js';

@customElement('t-select')
export class SelectElement extends LitElement {
  static formAssociated = true;

  static override styles = css`
    :host {
      display: inline-flex;
      font-size: 0.85rem;
    }

    slot {
      display: none;
    }

    select {
      appearance: base-select;
      font: inherit;
      color: inherit;
      background-color: var(--t-button-background-color);
      border: 1px solid var(--t-button-border-color);
      border-radius: var(--t-border-radius);
      padding: 0.25em 0.5em;
      cursor: default;
      user-select: none;
      min-width: 1.5em;
    }

    select:hover {
      background-color: var(--t-button-hover-background-color);
    }

    select:focus-visible {
      outline: 2px solid var(--t-button-focus-ring-color);
    }

    select:disabled {
      cursor: not-allowed;
      opacity: 0.5;
    }

    ::picker(select) {
      background-color: var(--ctp-macchiato-base);
      border: 1px solid var(--ctp-macchiato-surface0);
      border-radius: var(--t-border-radius);
      padding: 0;
      scrollbar-width: thin;
    }

    select::picker-icon {
      transition: 0.2s rotate;
    }

    select:open::picker-icon {
      rotate: -180deg;
    }

    option {
      padding: 0.25em 0.5em;
      background-color: var(--t-button-background-color);
      color: inherit;
      cursor: default;
    }

    option:hover {
      background-color: var(--t-button-hover-background-color);
    }

    option:checked {
      background-color: var(--t-button-active-background-color);
    }
  `;

  protected internals = this.attachInternals();

  @property({ reflect: true })
  value = '';

  @property({ type: Boolean })
  disabled = false;

  @property({ type: Boolean })
  required = false;

  @query('select')
  private selectEl!: HTMLSelectElement;

  @queryAssignedElements({ selector: 'option' })
  private slottedOptions!: HTMLOptionElement[];

  formResetCallback(): void {
    if (this.selectEl) {
      this.selectEl.selectedIndex = 0;
      this.value = this.selectEl.value;
    } else {
      this.value = '';
    }
    this.updateFormValue();
  }

  formDisabledCallback(disabled: boolean): void {
    this.disabled = disabled;
  }

  protected override updated(changedProperties: PropertyValues<this>): void {
    if (changedProperties.has('value') && this.selectEl) {
      this.selectEl.value = this.value;
      this.updateFormValue();
    }

    if (changedProperties.has('disabled')) {
      this.internals.ariaDisabled = String(this.disabled);
    }
  }

  private updateFormValue(): void {
    this.internals.setFormValue(this.value);

    if (this.required && !this.value) {
      this.internals.setValidity(
        { valueMissing: true },
        'Please select an option',
        this.selectEl,
      );
    } else {
      this.internals.setValidity({});
    }
  }

  private syncOptions(): void {
    if (!this.selectEl) return;

    const targetValue = this.value || this.selectEl.value;

    this.selectEl.replaceChildren(
      ...this.slottedOptions.map(
        (opt) => opt.cloneNode(true) as HTMLOptionElement,
      ),
    );

    this.selectEl.value = targetValue;
    this.value = this.selectEl.value;
    this.updateFormValue();
  }

  private handleChange(): void {
    this.value = this.selectEl.value;
    this.updateFormValue();
    this.dispatchEvent(new Event('change', { bubbles: true, composed: true }));
  }

  protected override render(): TemplateResult {
    return html`
      <slot @slotchange=${this.syncOptions}></slot>
      <select
        ?disabled=${this.disabled}
        ?required=${this.required}
        @change=${this.handleChange}
      ></select>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-select': SelectElement;
  }
}
