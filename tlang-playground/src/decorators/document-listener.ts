import { LitElement } from 'lit';
import { EventController } from '../controllers/host-listener';

export function documentListener(eventName: string): PropertyDecorator;
export function documentListener(eventNames: string[]): PropertyDecorator;
export function documentListener(
  eventNames: string | string[],
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
      );
    };
  };
}
