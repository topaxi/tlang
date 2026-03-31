const icons = {
  'chevron-right': '',
  'chevron-down': '',
  'radio-unchecked': '',
  'radio-checked': '',
  'lightning-on': '󰉁',
  'lightning-off': '󰛕',
  checkmark: '✓',
} as const;

export type IconName = keyof typeof icons;

export function icon(name: IconName): string {
  return icons[name];
}
