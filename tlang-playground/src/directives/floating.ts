import {
  autoUpdate,
  computePosition,
  ComputePositionConfig,
  flip,
  shift,
} from '@floating-ui/dom';
import { AsyncDirective } from 'lit/async-directive.js';
import {
  AttributePart,
  directive,
  DirectiveParameters,
  PartInfo,
  PartType,
} from 'lit/directive.js';

function roundByDPR(value: number) {
  const dpr = window.devicePixelRatio || 1;
  return Math.round(value * dpr) / dpr;
}

export interface FloatingOptions {
  anchor?: string;
  popover?: 'auto' | 'hint' | 'manual';
}

class FloatingDirective extends AsyncDirective {
  private cleanup: (() => void) | null = null;
  private listener: ((event: Event) => void) | null = null;
  private element: HTMLElement | null = null;

  constructor(partInfo: PartInfo) {
    super(partInfo);

    if (
      partInfo.type !== PartType.ATTRIBUTE ||
      partInfo.name !== 'popover' ||
      (partInfo.strings?.length as number) > 2
    ) {
      throw new Error(
        '`floating()` can only be used in the `popover` attribute ' +
          'and must be the only part in the attribute.',
      );
    }
  }

  private setup(referenceId: string | undefined, element: HTMLElement): void {
    if (this.listener != null) return;

    this.listener = (event) => {
      if ((event as ToggleEvent).newState === 'open') {
        this.autoUpdate(referenceId, element);
      } else {
        this.cleanup?.();
        this.cleanup = null;
      }
    };

    element.addEventListener('beforetoggle', this.listener);
  }

  private autoUpdate(anchor: string | undefined, element: HTMLElement) {
    if (this.cleanup != null) return;

    let selector = anchor ? `#${anchor}` : `[popovertarget="${element.id}"]`;
    let reference = () =>
      (element.getRootNode() as HTMLElement | ShadowRoot).querySelector(
        selector,
      )!;
    let positionOptions = {
      strategy: 'fixed',
      middleware: [flip(), shift({ padding: 4 })],
    } satisfies Partial<ComputePositionConfig>;

    if (!reference()) {
      throw new Error(`No reference element found with selector "${selector}"`);
    }

    this.cleanup = autoUpdate(reference(), element, () => {
      computePosition(reference(), element, positionOptions).then(
        ({ x, y }) => {
          element.style.margin = '0';
          element.style.top = '0';
          element.style.left = '0';
          element.style.transform = `translate(${roundByDPR(x)}px,${roundByDPR(y)}px)`;
        },
      );
    });
  }

  protected override disconnected(): void {
    this.cleanup?.();
    this.cleanup = null;
    this.element?.removeEventListener('beforetoggle', this.listener!);
  }

  override update(
    { element }: AttributePart,
    [popoverOrOptions]: DirectiveParameters<this>,
  ): ReturnType<typeof this.render> {
    this.element = element;

    this.setup(
      typeof popoverOrOptions === 'object'
        ? popoverOrOptions?.anchor
        : undefined,
      element,
    );

    return this.render(popoverOrOptions);
  }

  override render(
    popoverOrOptions?: FloatingOptions['popover'] | FloatingOptions,
  ): unknown {
    let popover =
      typeof popoverOrOptions === 'object'
        ? popoverOrOptions?.popover
        : popoverOrOptions;

    return popover ?? '';
  }
}

export const floating = directive(FloatingDirective);

export type { FloatingDirective };
