import { ReactiveController, ReactiveControllerHost } from 'lit';

interface EventTarget<E = Event> {
  addEventListener(
    eventName: string,
    listener: EventListenerOrEventListenerObject<E>,
  ): void;
  removeEventListener(
    eventName: string,
    listener: EventListenerOrEventListenerObject<E>,
  ): void;
}

interface EventListener<E = Event> {
  (evt: E): void;
}

interface EventListenerObject<E = Event> {
  handleEvent: EventListener<E>;
}

type EventListenerOrEventListenerObject<E = Event> =
  | EventListener<E>
  | EventListenerObject<E>;

export interface EventControllerHost<E = Event>
  extends ReactiveControllerHost,
    EventTarget<E>,
    EventListenerObject<E> {}

export class EventController<
  E = Event,
  Target extends EventTarget<E> = EventTarget<E>,
  Listener extends
    EventListenerOrEventListenerObject<E> = EventListenerOrEventListenerObject<E>,
> implements ReactiveController
{
  protected host: ReactiveControllerHost;
  protected eventNames: string[];
  protected listener: EventListenerObject<E>;

  constructor(
    host: ReactiveControllerHost,
    target: Target,
    eventName: string,
    listener?: Listener | null,
  );
  constructor(
    host: ReactiveControllerHost,
    target: Target,
    eventNames: string[],
    listener?: Listener | null,
  );
  constructor(
    host: ReactiveControllerHost,
    target: Target,
    eventNames: string | string[],
    listener?: Listener | null,
  );
  constructor(
    host: ReactiveControllerHost,
    protected target: Target,
    eventNames: string | string[],
    listener?: Listener | null,
  ) {
    this.eventNames = Array.isArray(eventNames) ? eventNames : [eventNames];
    this.listener =
      typeof listener === 'function'
        ? ({ handleEvent: listener.bind(host) } as EventListenerObject<E>)
        : (listener ?? this);
    (this.host = host).addController(this);
  }

  hostConnected(): void {
    for (let eventName of this.eventNames) {
      this.target.addEventListener(eventName, this);
    }
  }

  hostDisconnected(): void {
    for (let eventName of this.eventNames) {
      this.target.removeEventListener(eventName, this);
    }
  }

  handleEvent(event: E): void {
    if (this.listener !== this) {
      this.listener.handleEvent(event);
    }

    throw new Error(`${this.constructor.name}.handleEvent() not implemented`);
  }
}
