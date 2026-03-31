const icons = {
  'chevron-right': '',
  'chevron-down': '',
  'radio-unchecked': '',
  'radio-checked': '',
  'lightning-on': '󰉁',
  'lightning-off': '󰛕',
  checkmark: '✓',
  warning: '',
  error: '',
  trash: '',
  settings: '',
} as const;

export type IconName = keyof typeof icons;

export function icon(name: IconName): string {
  return icons[name];
}
