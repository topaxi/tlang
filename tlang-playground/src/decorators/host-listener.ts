import { LitElement } from 'lit';
import {
  EventController,
  EventListenerOptions,
} from '../controllers/event-controller';

export function hostListener(
  eventName: string,
  options?: EventListenerOptions,
): PropertyDecorator;
export function hostListener(
  eventNames: string[],
  options?: EventListenerOptions,
): PropertyDecorator;
export function hostListener(
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
        this,
        eventNames,
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        (this as any)[propertyKey],
        options,
      );
    };
  };
}
