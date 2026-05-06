import type { Extension } from '@codemirror/state';
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

export interface PreparedRename {
  from: number;
  to: number;
  placeholder: string;
}

export interface RenameEdit {
  from: number;
  to: number;
  insert: string;
}

export interface SignatureParameterInformation {
  label: string;
}

export interface SignatureInformation {
  label: string;
  parameters: SignatureParameterInformation[];
}

export interface SignatureHelp {
  signatures: SignatureInformation[];
  activeSignature: number;
  activeParameter: number;
}

export interface SemanticToken {
  from: number;
  to: number;
  type: string;
  modifiers?: string[];
}

export type HoverProvider = (pos: number) => HoverInfo | null;
export type GotoDefinitionProvider = (pos: number) => DefinitionLocation | null;
export type PrepareRenameProvider = (
  pos: number,
) => PreparedRename | null | Promise<PreparedRename | null>;
export type RenameProvider = (
  pos: number,
  newName: string,
) => RenameEdit[] | null | Promise<RenameEdit[] | null>;
export type RenamePrompt = (
  context: PreparedRename,
) => string | null | Promise<string | null>;
export type SignatureHelpProvider = (
  pos: number,
) => SignatureHelp | null | Promise<SignatureHelp | null>;
export type SemanticTokenProvider = (
  code: string,
) => SemanticToken[] | Promise<SemanticToken[]>;

export function tlangSignatureHelp(provider: SignatureHelpProvider): Extension;
export function tlangRename(
  view: import('@codemirror/view').EditorView,
  options: {
    prepareRename: PrepareRenameProvider;
    rename: RenameProvider;
    requestName: RenamePrompt;
    pos?: number;
  },
): Promise<boolean>;
export function tlangSemanticTokens(
  source: SemanticToken[] | SemanticTokenProvider,
  config?: { debounceMs?: number },
): Extension;

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
  signatureHelpProvider?: SignatureHelpProvider;
  semanticTokens?: SemanticToken[];
  semanticTokenProvider?: SemanticTokenProvider;
  semanticTokenDebounceMs?: number;
}): LanguageSupport;
