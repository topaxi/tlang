import { CSSResultGroup } from 'lit';

export function styles(...styles: CSSResultGroup[]): CSSResultGroup {
  return styles.flat();
}
