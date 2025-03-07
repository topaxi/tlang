import { LitElement } from 'lit';
import { MediaController } from '../controllers/media-controller';

export function mediaQuery(query: string): PropertyDecorator {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  return function (target: any, propertyKey) {
    let controller: MediaController | null = null;

    Object.defineProperty(target, propertyKey, {
      get(this: LitElement) {
        controller ??= new MediaController(this, query);
        return controller.matches;
      },
    });
  };
}
