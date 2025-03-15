export const isMac = (): boolean =>
  navigator.userAgent.toLowerCase().includes('macintosh');

export const isWin = (): boolean =>
  navigator.userAgent.toLowerCase().includes('windows');

export const platform = (): 'mac' | 'win' | 'other' =>
  isMac() ? 'mac' : isWin() ? 'win' : 'other';
