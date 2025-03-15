import { capitalize } from './capitalize';
import { isMac, isWin, platform } from './platform-detection';

// prettier-ignore
type Alphabet = | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z';
type Digit = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
type Ulphabet = Uppercase<Alphabet>;
type Letter = Alphabet | Ulphabet | Digit;

const SHORTCUT_MODIFIERS = ['ctrl', 'shift', 'alt', 'meta', 'cmd'] as const;

export type ShortcutModifier = (typeof SHORTCUT_MODIFIERS)[number];

function isShortcutModifier(key: string): key is ShortcutModifier {
  return SHORTCUT_MODIFIERS.includes(key as ShortcutModifier);
}

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
  cmd: 'Command',
} as const;

interface ParseShortcutDefinitionOptions {
  mapCtrlToMeta?: boolean;
}

export function parseShortcutDefinition(
  def: ShortcutDefinition,
  options: ParseShortcutDefinitionOptions = {},
): Keyshortcuts {
  const { mapCtrlToMeta = isMac() } = options;

  const parts = def.split('+');

  const result: Keyshortcuts = {
    ctrl: false,
    shift: false,
    alt: false,
    meta: false,
    key: '',
  };

  for (const part of parts) {
    if (isShortcutModifier(part)) {
      if (part === 'cmd') {
        result.meta = true;
      } else if (part === 'ctrl' && mapCtrlToMeta) {
        result.meta = true;
      } else {
        result[part] = true;
      }
    } else {
      result.key = capitalize(part);
    }
  }

  return result;
}

export function parseMacShortcutDefinition(
  def: ShortcutDefinition,
  options?: Omit<ParseShortcutDefinitionOptions, 'mapCtrlToMeta'>,
): Keyshortcuts {
  // Explicit mac shortcuts do not need to map ctrl, they migth want to use
  // ctrl on their own.
  return parseShortcutDefinition(def, { ...options, mapCtrlToMeta: false });
}

export function toAriaKeyshortcuts(keyshortcuts: Keyshortcuts): string {
  let str = '';
  for (const [key, value] of Object.entries(keyshortcuts)) {
    if (value && key in modifierMap) {
      if (key === 'meta' && isMac()) {
        str += `${modifierMap.cmd}+`;
      } else if (key === 'meta' && isWin()) {
        str += `Win+`;
      } else {
        str += `${modifierMap[key as ShortcutModifier]}+`;
      }
    }
  }
  return str + keyshortcuts.key;
}

let prettyMapping = {
  mac: {
    ctrl: '⌃',
    shift: '⇧',
    alt: '⌥',
    meta: '⌘',
    cmd: '⌘',
  },
  win: {
    ctrl: 'Ctrl',
    shift: 'Shift',
    alt: 'Alt',
    meta: 'Win',
    cmd: 'Win',
  },
  other: {
    ctrl: 'Ctrl',
    shift: 'Shift',
    alt: 'Alt',
    meta: 'Meta',
    cmd: 'Meta',
  },
};

export function formatKeyshortcutsPretty(keyshortcuts: Keyshortcuts): string {
  let str = '';
  for (const [key, value] of Object.entries(keyshortcuts)) {
    if (value && key in modifierMap) {
      str += prettyMapping[platform()][key as ShortcutModifier];
      str += '+';
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

export function eventMatchesShortcutDefinition(
  e: KeyboardEvent,
  def: ShortcutDefinition,
  options?: ParseShortcutDefinitionOptions,
): boolean {
  return eventMatchesShortcut(e, parseShortcutDefinition(def, options));
}

export function eventMatchesMacShortcutDefinition(
  e: KeyboardEvent,
  def: ShortcutDefinition,
  options?: ParseShortcutDefinitionOptions,
): boolean {
  return eventMatchesShortcut(e, parseMacShortcutDefinition(def, options));
}
