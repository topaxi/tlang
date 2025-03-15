import { LitElement } from 'lit';
import {
  EventController,
  EventListenerOptions,
} from '../controllers/event-controller';

export function documentListener(
  eventName: string,
  options?: EventListenerOptions,
): PropertyDecorator;
export function documentListener(
  eventNames: string[],
  options?: EventListenerOptions,
): PropertyDecorator;
export function documentListener(
  eventNames: string | string[],
  options?: EventListenerOptions,
): PropertyDecorator {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return function (target: any, propertyKey) {
    let firstUpdated = target.firstUpdated;

    target.firstUpdated = function (this: LitElement, ...args: unknown[]) {
      firstUpdated.apply(this, args);
      new EventController(
        this,
        this.ownerDocument,
        eventNames,
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (this as any)[propertyKey],
        options,
      );
    };
  };
}
