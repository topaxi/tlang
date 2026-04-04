import { Language, LanguageSupport, LRLanguage } from '@codemirror/language';

export const tlangLanguage: LRLanguage;

export function tlangLanguageSupport(options?: {
  reLanguage?: Language;
  htmlLanguage?: Language;
  cssLanguage?: Language;
  sqlLanguage?: Language;
  jsonLanguage?: Language;
  jsLanguage?: Language;
  markdownLanguage?: Language;
}): LanguageSupport;
