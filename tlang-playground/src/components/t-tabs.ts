import { css, html, LitElement, PropertyValues, TemplateResult } from 'lit';
import {
  customElement,
  property,
  queryAssignedElements,
} from 'lit/decorators.js';
import { hostListener } from '../decorators/host-listener';

@customElement('t-tabs')
export class TabsElement extends LitElement {
  static override styles = css`
    :host {
      --tabs-focus-ring-color: var(--ctp-macchiato-surface0);

      display: flex;
      flex-direction: column;
    }

    [role='tablist'] {
      display: flex;
      border-bottom: 1px solid var(--ctp-macchiato-surface0);
    }

    [role='tablist']:focus-visible {
      outline: 2px solid var(--tabs-focus-ring-color);
      outline-offset: -2px;
    }

    ::slotted([slot='tab'])::after {
      content: '';
      position: absolute;
      top: 0;
      right: 0;
      bottom: 0;
      border-right: 1px solid var(--ctp-macchiato-surface0);
    }

    ::slotted(t-tab-panel) {
      position: relative;
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

  @hostListener(['keyup', 't-tab-select'])
  handleEvent(e: Event) {
    let event = e as
      | (KeyboardEvent & { type: `key${string}` })
      | (CustomEvent<{ id: string }> & { type: 't-tab-select' });

    switch (event.type) {
      case 't-tab-select': {
        this.selected = event.detail.id;
        break;
      }
      case 'keyup': {
        if (event.key !== 'ArrowLeft' && event.key !== 'ArrowRight') {
          return;
        }

        let index = this.tabs.findIndex((tab) => tab.id === this.selected);
        let direction = event.key === 'ArrowLeft' ? -1 : 1;

        let next = this.tabs[index + direction];

        if (next) {
          event.preventDefault();
          next.select();
        }
        break;
      }
    }
  }

  protected override updated(changedProperties: PropertyValues<this>): void {
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

  protected override render(): TemplateResult {
    return html`
      <slot part="tablist" role="tablist" name="tab" tabindex="0"></slot>
      <slot></slot>
    `;
  }
}

@customElement('t-tab')
export class TabElement extends LitElement {
  static override styles = css`
    :host {
      position: relative;
      border-bottom: 1px solid transparent;
      margin-bottom: -1px;
      user-select: none;
    }

    :host([aria-selected='true']) {
      color: var(--ctp-macchiato-teal);
      border-color: 1px solid var(--ctp-macchiato-teal);
    }

    button {
      all: unset;
      appearance: none;
      cursor: pointer;
      padding: 0 1ch;
    }
  `;

  @property({ type: String, reflect: true })
  override role = 'tab';

  @property({ type: Boolean })
  selected = false;

  protected override updated(changedProperties: PropertyValues<this>): void {
    if (changedProperties.has('selected')) {
      this.setAttribute('aria-selected', String(this.selected));
    }
  }

  select() {
    this.dispatchEvent(
      new CustomEvent('t-tab-select', {
        bubbles: true,
        composed: true,
        detail: this,
      }),
    );
  }

  protected override render(): TemplateResult {
    return html`
      <button part="button" @click=${this.select} tabindex="-1">
        <slot></slot>
      </button>
    `;
  }
}

@customElement('t-tab-panel')
export class TabPanelElement extends LitElement {
  @property({ type: String, reflect: true })
  name: string | undefined;

  @property({ type: String, reflect: true })
  override role = 'tabpanel';

  protected override createRenderRoot() {
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
