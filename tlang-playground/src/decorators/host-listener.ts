import { LitElement } from 'lit';
import { EventController } from '../controllers/event-controller';

export function hostListener(eventName: string): PropertyDecorator;
export function hostListener(eventNames: string[]): PropertyDecorator;
export function hostListener(eventNames: string | string[]): PropertyDecorator {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return function (target: any, propertyKey) {
    let firstUpdated = target.firstUpdated;

    target.firstUpdated = function (this: LitElement, ...args: unknown[]) {
      firstUpdated.apply(this, args);
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      new EventController(this, this, eventNames, (this as any)[propertyKey]);
    };
  };
}
