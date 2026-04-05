import { Language, LanguageSupport, LRLanguage } from '@codemirror/language';

export const tlangLanguage: LRLanguage;

export interface HoverInfo {
  text: string;
  from: number;
  to: number;
}

export interface DefinitionLocation {
  from: number;
  to: number;
}

export type HoverProvider = (pos: number) => HoverInfo | null;
export type GotoDefinitionProvider = (pos: number) => DefinitionLocation | null;

export function tlangLanguageSupport(options?: {
  reLanguage?: Language;
  htmlLanguage?: Language;
  cssLanguage?: Language;
  sqlLanguage?: Language;
  jsonLanguage?: Language;
  jsLanguage?: Language;
  markdownLanguage?: Language;
  hoverProvider?: HoverProvider;
  gotoDefinitionProvider?: GotoDefinitionProvider;
}): LanguageSupport;
