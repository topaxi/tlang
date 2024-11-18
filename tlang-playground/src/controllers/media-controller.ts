import { ReactiveController, ReactiveControllerHost } from 'lit';

export class MediaController implements ReactiveController {
  private host: ReactiveControllerHost;
  private mediaQueryList: MediaQueryList | null = null;

  get matches(): boolean {
    return this.mediaQueryList?.matches === true;
  }

  constructor(
    host: ReactiveControllerHost,
    private query: string,
  ) {
    (this.host = host).addController(this);
  }

  hostConnected(): void {
    this.mediaQueryList = window.matchMedia(this.query);
    this.mediaQueryList.addEventListener('change', this);
  }

  hostDisconnected(): void {
    this.mediaQueryList?.removeEventListener('change', this);
    this.mediaQueryList = null;
  }

  handleEvent(_event: MediaQueryListEvent): void {
    this.host.requestUpdate();
  }
}
