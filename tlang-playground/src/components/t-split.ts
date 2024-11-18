import { css, html, LitElement } from 'lit';
import { customElement, property, query } from 'lit/decorators.js';
import { throttleAnimationFrame } from '../utils/debounce';

@customElement('t-split')
export class SplitElement extends LitElement {
  static styles = css`
    :host {
      display: flex;
      height: 100%;
    }

    :host > [part='handle'] {
      appearance: none;
      border: 0;
      flex-shrink: 0;
      margin: 0;
      background-color: var(--ctp-macchiato-surface0);
    }

    :host([direction='horizontal']) {
      flex-direction: column;
    }

    :host([direction='vertical']) {
      flex-direction: row;
    }

    :host([direction='horizontal']) > [part='handle'] {
      height: 8px;
      cursor: row-resize;
    }

    :host([direction='vertical']) > [part='handle'] {
      width: 8px;
      cursor: col-resize;
    }

    ::slotted(*) {
      flex: 1 0 100px;
      overflow: auto;
    }
  `;

  @property()
  direction: 'horizontal' | 'vertical' = 'horizontal';

  @query('[part="handle"]', true)
  private handle!: HTMLHRElement;

  @query('slot:first-child', true)
  private firstSlot!: HTMLSlotElement;

  private get firstSlottedElement() {
    return this.firstSlot.assignedElements()[0] as HTMLElement;
  }

  constructor() {
    super();
    this.updateSlotSize = throttleAnimationFrame(this.updateSlotSize);
  }

  private documentEventHandler = {
    handleEvent: (event: MouseEvent) => {
      this.handleEvent(event);
    },
  };

  protected handleEvent(event: MouseEvent) {
    switch (event.type) {
      case 'mousedown': {
        this.ownerDocument.addEventListener(
          'mousemove',
          this.documentEventHandler,
        );
        this.ownerDocument.addEventListener(
          'mouseup',
          this.documentEventHandler,
          { once: true },
        );
        break;
      }
      case 'mouseup': {
        this.ownerDocument.removeEventListener(
          'mousemove',
          this.documentEventHandler,
        );
        break;
      }
      case 'mousemove': {
        let containerRect = this.getBoundingClientRect();
        let handleRect = this.handle.getBoundingClientRect();

        if (this.direction === 'horizontal') {
          let mousePosition = Math.min(
            Math.max(event.clientY - containerRect.top, 0),
            containerRect.height - handleRect.height,
          );

          this.updateSlotSize(mousePosition);
        } else {
          let mousePosition = Math.min(
            Math.max(event.clientX - containerRect.left, 0),
            containerRect.width - handleRect.width,
          );

          this.updateSlotSize(mousePosition);
        }
      }
    }
  }

  protected updateSlotSize(size: number) {
    if (this.direction === 'horizontal') {
      this.firstSlottedElement.style.minHeight = `${size}px`;
      this.firstSlottedElement.style.maxHeight = `${size}px`;
    } else {
      this.firstSlottedElement.style.minWidth = `${size}px`;
      this.firstSlottedElement.style.maxWidth = `${size}px`;
    }
  }

  protected render() {
    return html`
      <slot name="first"></slot>
      <hr part="handle" @mousedown=${this} />
      <slot name="second"></slot>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-split': SplitElement;
  }
}
