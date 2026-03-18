import { Language, LanguageSupport } from '@codemirror/language';

export function tlangLanguageSupport(options?: {
  reLanguage?: Language;
  htmlLanguage?: Language;
  cssLanguage?: Language;
  sqlLanguage?: Language;
  jsonLanguage?: Language;
  jsLanguage?: Language;
}): LanguageSupport;
