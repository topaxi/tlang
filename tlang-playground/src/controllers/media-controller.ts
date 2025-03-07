import { ReactiveController, ReactiveControllerHost } from 'lit';
import { EventController } from './host-listener';

export class MediaController
  extends EventController<MediaQueryListEvent, MediaQueryList>
  implements ReactiveController
{
  get matches(): boolean {
    return this.target?.matches === true;
  }

  constructor(host: ReactiveControllerHost, query: string) {
    super(host, window.matchMedia(query), 'change');
  }

  override handleEvent(_event: MediaQueryListEvent): void {
    this.host.requestUpdate();
  }
}
