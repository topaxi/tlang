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

    slot {
      display: block;
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

  private startMouseResizing() {
    this.ownerDocument.addEventListener('mousemove', this);
    this.ownerDocument.addEventListener('mouseup', this, { once: true });
  }

  private stopMouseResizing() {
    this.ownerDocument.removeEventListener('mousemove', this);
  }

  private resizeToMouse(position: { clientX: number; clientY: number }) {
    let containerRect = this.getBoundingClientRect();
    let handleRect = this.handle.getBoundingClientRect();

    if (this.direction === 'horizontal') {
      let firstSlotSize = Math.min(
        Math.max(position.clientY - containerRect.top, 0),
        containerRect.height - handleRect.height / 2,
      );
      let secondSlotSize = containerRect.height - firstSlotSize;

      this.updateSlotSize(firstSlotSize, secondSlotSize);
    } else {
      let firstSlotSize = Math.min(
        Math.max(position.clientX - containerRect.left, 0),
        containerRect.width - handleRect.width / 2,
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

  private startTouchResizing(event: TouchEvent) {
    let [touch] = event.touches;

    // Assuming that while resizing the container itself will not change size.
    let containerRect = this.getBoundingClientRect();
    let handleRect = this.handle.getBoundingClientRect();

    let dimension: 'width' | 'height' =
      this.direction === 'horizontal' ? 'height' : 'width';
    let direction: 'pageX' | 'pageY' =
      this.direction === 'horizontal' ? 'pageY' : 'pageX';
    let offsetDirection: 'x' | 'y' =
      this.direction === 'horizontal' ? 'y' : 'x';

    let containerSize = containerRect[dimension];
    let initialFirstSlotSize =
      this.firstContainer.getBoundingClientRect()[dimension];

    let start = { x: touch[direction], y: touch[direction] };
    let offset = { x: 0, y: 0 };

    let touchmove = (event: TouchEvent) => {
      let [touch] = event.touches;

      offset[offsetDirection] = start[offsetDirection] - touch[direction];

      let firstSlotSize = Math.min(
        containerSize - handleRect[dimension] / 2,
        initialFirstSlotSize - offset[offsetDirection],
      );
      let secondSlotSize = containerSize - firstSlotSize;

      this.updateSlotSize(firstSlotSize, secondSlotSize);
    };

    let touchend = (_event: TouchEvent) => {
      this.ownerDocument.removeEventListener('touchmove', touchmove);
      this.ownerDocument.removeEventListener('touchcancel', touchend);
      this.ownerDocument.removeEventListener('touchend', touchend);
    };

    this.ownerDocument.addEventListener('touchmove', touchmove);
    this.ownerDocument.addEventListener('touchcancel', touchend);
    this.ownerDocument.addEventListener('touchend', touchend);
  }

  handleEvent(e: Event) {
    let event = e as
      | (MouseEvent & { type: 'dblclick' | `mouse${string}` })
      | (KeyboardEvent & { type: `key${string}` })
      | (TouchEvent & { type: `touch${string}` });

    switch (event.type) {
      case 'dblclick': {
        this.reset();
        break;
      }
      case 'mousedown': {
        if (event.button === 0) {
          this.startMouseResizing();
        }
        break;
      }
      case 'mouseup': {
        this.stopMouseResizing();
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
      case 'touchstart': {
        this.startTouchResizing(event);
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
      <slot name="first" part="first" id="first" role="region"></slot>
      <hr
        part="handle"
        tabindex="0"
        aria-controls="first"
        aria-orientation=${this.direction}
        @dblclick=${this}
        @mousedown=${this}
        @touchstart=${this}
        @keyup=${this}
      />
      <slot name="second" part="second" role="region"></slot>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    't-split': SplitElement;
  }
}
