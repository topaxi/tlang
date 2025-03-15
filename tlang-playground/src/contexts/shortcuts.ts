import { createContext } from '@lit/context';

export interface ShortcutsContextValue {
  showHints: boolean;
}

export const shortcutsContext = createContext(Symbol('shortcuts-context'));
