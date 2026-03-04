import { ReactiveController, ReactiveControllerHost } from 'lit';

export interface EventListenerOptions {
  capture?: boolean;
  once?: boolean;
  passive?: boolean;
  signal?: AbortSignal;
}

interface EventTarget<E = Event> {
  addEventListener(
    eventName: string,
    listener: EventListenerOrEventListenerObject<E>,
    options?: unknown,
  ): void;
  addEventListener(
    eventName: string,
    listener: EventListenerOrEventListenerObject<E>,
    options: EventListenerOptions,
  ): void;
  removeEventListener(
    eventName: string,
    listener: EventListenerOrEventListenerObject<E>,
    options?: unknown,
  ): void;
  removeEventListener(
    eventName: string,
    listener: EventListenerOrEventListenerObject<E>,
    options: EventListenerOptions,
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
  extends ReactiveControllerHost, EventTarget<E>, EventListenerObject<E> {}

export class EventController<
  E = Event,
  Target extends EventTarget<E> = EventTarget<E>,
  Listener extends EventListenerOrEventListenerObject<E> =
    EventListenerOrEventListenerObject<E>,
> implements ReactiveController {
  protected host: ReactiveControllerHost;
  protected eventNames: string[];
  protected listener: EventListenerObject<E>;

  constructor(
    host: ReactiveControllerHost,
    target: Target,
    eventName: string,
    listener?: Listener | null,
    options?: EventListenerOptions,
  );
  constructor(
    host: ReactiveControllerHost,
    target: Target,
    eventNames: string[],
    listener?: Listener | null,
    options?: EventListenerOptions,
  );
  constructor(
    host: ReactiveControllerHost,
    target: Target,
    eventNames: string | string[],
    listener?: Listener | null,
    options?: EventListenerOptions,
  );
  constructor(
    host: ReactiveControllerHost,
    protected target: Target,
    eventNames: string | string[],
    listener?: Listener | null,
    protected options?: EventListenerOptions,
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
      this.target.addEventListener(eventName, this, this.options);
    }
  }

  hostDisconnected(): void {
    for (let eventName of this.eventNames) {
      this.target.removeEventListener(eventName, this, this.options);
    }
  }

  handleEvent(event: E): void {
    if (this.listener !== this) {
      return this.listener.handleEvent(event);
    }

    throw new Error(`${this.constructor.name}.handleEvent() not implemented`);
  }
}
