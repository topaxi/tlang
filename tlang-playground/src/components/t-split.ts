import { css, html, LitElement, TemplateResult } from 'lit';
import { customElement, property, query } from 'lit/decorators.js';
import { throttleAnimationFrame } from '../utils/debounce';

export type SplitDirection = 'horizontal' | 'vertical';

@customElement('t-split')
export class SplitElement extends LitElement implements EventListenerObject {
  static styles = css`
    :host {
      --handle-size: 8px;
      --handle-color: var(--ctp-macchiato-surface0);
      --handle-hover-color: var(--ctp-macchiato-surface1);
      --handle-focus-color: var(--ctp-macchiato-surface1);

      --split-ratio: 0.5;

      --flex-basis: calc(var(--split-ratio) * 100% - var(--handle-size) / 2);

      display: flex;
      height: 100%;
    }

    :host([direction='horizontal']) {
      flex-direction: column;
    }

    :host([direction='vertical']) {
      flex-direction: row;
    }

    hr {
      appearance: none;
      border: 0;
      flex-shrink: 0;
      margin: 0;
      background-color: var(--handle-color);
    }

    hr:hover {
      background-color: var(--handle-hover-color);
    }

    hr:focus-visible {
      background-color: var(--handle-focus-color);
    }

    :host([direction='horizontal']) > hr {
      height: var(--handle-size);
      cursor: row-resize;
    }

    :host([direction='vertical']) > hr {
      width: var(--handle-size);
      cursor: col-resize;
    }

    div {
      flex: 1 0 var(--flex-basis);
      overflow: auto;
    }
  `;

  @property()
  direction: SplitDirection = 'horizontal';

  @query('[part="handle"]', true)
  private handle!: HTMLHRElement;

  @query('[part="first"]', true)
  private firstContainer!: HTMLDivElement;

  @query('[part="second"]', true)
  private secondContainer!: HTMLDivElement;

  constructor() {
    super();
    this.updateSlotSize = throttleAnimationFrame(this.updateSlotSize);
  }

  reset(): void {
    this.firstContainer.removeAttribute('style');
    this.secondContainer.removeAttribute('style');
  }

  private startResizing() {
    this.ownerDocument.addEventListener('mousemove', this);
    this.ownerDocument.addEventListener('mouseup', this, { once: true });
  }

  private stopResizing() {
    this.ownerDocument.removeEventListener('mousemove', this);
  }

  private resizeToMouse(position: { clientX: number; clientY: number }) {
    let containerRect = this.getBoundingClientRect();
    let handleRect = this.handle.getBoundingClientRect();

    if (this.direction === 'horizontal') {
      let firstSlotSize = Math.min(
        Math.max(position.clientY - containerRect.top, 0),
        containerRect.height - handleRect.height,
      );
      let secondSlotSize = containerRect.height - firstSlotSize;

      this.updateSlotSize(firstSlotSize, secondSlotSize);
    } else {
      let firstSlotSize = Math.min(
        Math.max(position.clientX - containerRect.left, 0),
        containerRect.width - handleRect.width,
      );
      let secondSlotSize = containerRect.width - firstSlotSize;

      this.updateSlotSize(firstSlotSize, secondSlotSize);
    }
  }

  private resizeByPercentage(percentage: number) {
    let containerRect = this.getBoundingClientRect();
    let dimension: 'width' | 'height' =
      this.direction === 'horizontal' ? 'height' : 'width';
    let containerSize = containerRect[dimension];

    let currentFirstSlotSize =
      this.firstContainer.getBoundingClientRect()[dimension];

    let firstSlotSize = Math.min(
      containerSize,
      currentFirstSlotSize + containerSize * percentage,
    );
    let secondSlotSize = containerSize - firstSlotSize;

    this.updateSlotSize(firstSlotSize, secondSlotSize);
  }

  handleEvent(e: Event) {
    let event = e as
      | (MouseEvent & { type: 'dblclick' | `mouse${string}` })
      | (KeyboardEvent & { type: `key${string}` });

    switch (event.type) {
      case 'dblclick': {
        this.reset();
        break;
      }
      case 'mousedown': {
        if (event.button === 0) {
          this.startResizing();
        }
        break;
      }
      case 'mouseup': {
        this.stopResizing();
        break;
      }
      case 'mousemove': {
        this.resizeToMouse(event);
        break;
      }
      case 'keyup': {
        let increase =
          this.direction === 'horizontal' ? 'ArrowDown' : 'ArrowRight';
        let decrease =
          this.direction === 'horizontal' ? 'ArrowUp' : 'ArrowLeft';

        switch (event.key) {
          case increase: {
            this.resizeByPercentage(0.1);
            break;
          }
          case decrease: {
            this.resizeByPercentage(-0.1);
            break;
          }
        }
        break;
      }
    }
  }

  private updateSlotSize(firstSlotSize: number, secondSlotSize: number): void {
    this.setSlotSize(this.firstContainer, firstSlotSize);
    this.setSlotSize(this.secondContainer, secondSlotSize);
  }

  private setSlotSize(slot: HTMLDivElement, size: number) {
    let property = this.direction === 'horizontal' ? 'height' : 'width';

    slot.style.setProperty(
      `min-${property}`,
      `calc(${size}px - var(--handle-size) / 2)`,
      'important',
    );
    slot.style.setProperty(
      `max-${property}`,
      `calc(${size}px - var(--handle-size) / 2)`,
      'important',
    );
  }

  protected render(): TemplateResult {
    return html`
      <div part="first" id="first"><slot name="first"></slot></div>
      <hr
        part="handle"
        tabindex="0"
        aria-controls="first"
        aria-orientation=${this.direction}
        @dblclick=${this}
        @mousedown=${this}
        @keyup=${this}
      />
      <div part="second"><slot name="second"></slot></div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-split': SplitElement;
  }
}
