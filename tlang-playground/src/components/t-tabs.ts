import { css, html, LitElement, PropertyValues, TemplateResult } from 'lit';
import {
  customElement,
  property,
  queryAssignedElements,
} from 'lit/decorators.js';

@customElement('t-tabs')
export class TabsElement extends LitElement {
  static styles = css`
    :host {
      display: flex;
      flex-direction: column;
    }

    [role='tablist'] {
      display: flex;
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
    }

    ::slotted([slot='tab'])::after {
      content: '';
      position: absolute;
      top: 0;
      right: 0;
      bottom: 0;
      border-right: 1px solid var(--ctp-macchiato-surface0);
    }
  `;

  @queryAssignedElements({ slot: 'tab', selector: 't-tab' })
  tabs!: TabElement[];

  @queryAssignedElements({ selector: 't-tab-panel' })
  panels!: TabPanelElement[];

  @property({ type: String, reflect: true })
  selected: string | undefined;

  @property({ type: Boolean, reflect: true })
  single = false;

  constructor() {
    super();

    this.addEventListener('t-tab-select', this.handleTabSelect);
  }

  private handleTabSelect(event: Event) {
    event.stopPropagation();

    this.selected = (event as CustomEvent<{ id: string }>).detail.id;
  }

  protected updated(changedProperties: PropertyValues): void {
    if (changedProperties.has('selected')) {
      for (let tab of this.tabs) {
        tab.selected = tab.id === this.selected;
      }

      if (this.single) {
        let [panel] = this.panels;

        panel.setAttribute('name', this.selected!);
        panel.setAttribute('aria-labelledby', this.selected!);
      } else {
        for (let panel of this.panels) {
          panel.hidden = panel.name !== this.selected;
        }
      }
    }
  }

  protected render(): TemplateResult {
    return html`
      <div part="tablist" role="tablist">
        <slot name="tab"></slot>
      </div>
      <slot></slot>
    `;
  }
}

@customElement('t-tab')
export class TabElement extends LitElement {
  static styles = css`
    :host {
      position: relative;
      border-bottom: 1px solid transparent;
      margin-bottom: -1px;
      padding: 0 1ch;
    }

    :host([aria-selected='true']) {
      color: var(--ctp-macchiato-teal);
      border-color: 1px solid var(--ctp-macchiato-teal);
    }

    button {
      all: unset;
      appearance: none;
      cursor: pointer;
    }
  `;

  @property({ type: String, reflect: true })
  role = 'tab';

  @property({ type: Boolean })
  selected = false;

  protected updated(changedProperties: PropertyValues): void {
    if (changedProperties.has('selected')) {
      this.setAttribute('aria-selected', String(this.selected));
    }
  }

  select() {
    this.dispatchEvent(
      new CustomEvent('t-tab-select', {
        bubbles: true,
        composed: true,
        detail: { id: this.id },
      }),
    );
  }

  protected render(): TemplateResult {
    return html`
      <button part="button" @click=${this.select}>
        <slot></slot>
      </button>
    `;
  }
}

@customElement('t-tab-panel')
export class TabPanelElement extends LitElement {
  @property({ type: String, reflect: true })
  name: string | undefined;

  @property({ type: String, attribute: 'role', reflect: true })
  role = 'tabpanel';

  protected createRenderRoot() {
    return this;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-tabs': TabsElement;
    't-tab': TabElement;
    't-tab-panel': TabPanelElement;
  }
}
