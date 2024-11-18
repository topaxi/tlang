/* eslint-disable @typescript-eslint/no-explicit-any */

export function debounce<T extends (...args: any[]) => any>(
  func: T,
  wait: number,
): (...args: Parameters<T>) => void {
  let timeout: number;
  return function (this: unknown, ...args: unknown[]) {
    clearTimeout(timeout);
    timeout = window.setTimeout(() => func.apply(this, args), wait);
  };
}

export function throttle<T extends (...args: any[]) => any>(
  func: T,
  wait: number,
): (...args: Parameters<T>) => void {
  let timeout: number;
  return function (this: unknown, ...args: unknown[]) {
    if (!timeout) {
      timeout = window.setTimeout(() => {
        func.apply(this, args);
        timeout = 0;
      }, wait);
    }
  };
}

export function throttleAnimationFrame<T extends (...args: any[]) => any>(
  func: T,
): (...args: Parameters<T>) => void {
  let raf: number;
  return function (this: unknown, ...args: unknown[]) {
    if (!raf) {
      raf = window.requestAnimationFrame(() => {
        func.apply(this, args);
        raf = 0;
      });
    }
  };
}
