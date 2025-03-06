import { capitalize } from './capitalize';

// prettier-ignore
type Alphabet = | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z';
type Digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
type Ulphabet = Uppercase<Alphabet>;
type Letter = Alphabet | Ulphabet | Digit;

const SHORTCUT_MODIFIERS = ['ctrl', 'shift', 'alt', 'meta'] as const;

export type ShortcutModifier = (typeof SHORTCUT_MODIFIERS)[number];

export type ShortcutDefinition =
  | `${ShortcutModifier}+${ShortcutModifier}+${Letter}`
  | `${ShortcutModifier}+${Letter}`;

interface Keyshortcuts {
  ctrl: boolean;
  shift: boolean;
  alt: boolean;
  meta: boolean;
  key: string;
}

const modifierMap = {
  ctrl: 'Control',
  shift: 'Shift',
  alt: 'Alt',
  meta: 'Meta',
};

export function parseShortcutDefinition(def: ShortcutDefinition): Keyshortcuts {
  const parts = def.split('+');

  const result: Keyshortcuts = {
    ctrl: false,
    shift: false,
    alt: false,
    meta: false,
    key: '',
  };

  for (const part of parts) {
    if (SHORTCUT_MODIFIERS.includes(part as ShortcutModifier)) {
      result[part as ShortcutModifier] = true;
    } else {
      result.key = capitalize(part);
    }
  }

  return result;
}

export function toAriaKeyshortcuts(keyshortcuts: Keyshortcuts): string {
  let str = '';
  for (const [key, value] of Object.entries(keyshortcuts)) {
    if (value && key in modifierMap) {
      str += `${modifierMap[key as ShortcutModifier]}+`;
    }
  }
  return str + keyshortcuts.key;
}

export function eventMatchesShortcut(
  e: KeyboardEvent,
  keyshortcuts: Keyshortcuts,
): boolean {
  return (
    e.ctrlKey === keyshortcuts.ctrl &&
    e.shiftKey === keyshortcuts.shift &&
    e.altKey === keyshortcuts.alt &&
    e.metaKey === keyshortcuts.meta &&
    capitalize(e.key) === keyshortcuts.key
  );
}
