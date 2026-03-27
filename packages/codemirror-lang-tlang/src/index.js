import { parser } from './parser.js';
import { foldNodeProp, foldInside, indentNodeProp } from '@codemirror/language';
import { styleTags, tags as t } from '@lezer/highlight';
import { parseMixed } from '@lezer/common';

let parserWithMetadata = parser.configure({
  props: [
    styleTags({
      VariableName: t.variableName,
      'VariableName/self': t.self,
      VariableDefinition: t.definition(t.variableName),
      Namespace: t.namespace,
      'PathExpression/PathName': t.variableName,
      'CallExpression/PathExpression/PathName': t.function(t.variableName),
      'FunctionName/FunctionDefinition': t.function(t.variableName),
      'FunctionDeclaration/FunctionName/FunctionDefinition': t.function(
        t.definition(t.variableName),
      ),
      'FunctionArity/...': t.number,
      'CallExpression/VariableName': t.function(t.variableName),
      'CallExpression/MemberExpression/PropertyName': t.function(
        t.propertyName,
      ),
      'if else return rec break continue loop for match': t.controlKeyword,
      'let enum fn struct protocol impl pub': t.definitionKeyword,
      'in with apply': t.keyword,
      'not and or': t.logicOperator,
      PropertyName: t.propertyName,
      PathSep: t.punctuation,
      'ProtocolDeclaration/...': t.typeName,
      'ImplBlock/...': t.typeName,
      'ApplyStatement/identifier': t.function(t.propertyName),

      'StructDeclaration/...': t.typeName,
      'EnumVariant/...': t.typeName,
      'EnumPattern/...': t.typeName,
      'StructPattern/...': t.typeName,
      BooleanLiteral: t.bool,
      WildcardPattern: t.special(t.variableName),
      WildcardExpression: t.special(t.variableName),
      Number: t.number,
      String: t.string,
      TagStringTag: t.special(t.regexp),
      'TaggedStringOpenDouble TaggedStringOpenSingle TaggedStringDoubleClose TaggedStringSingleClose':
        t.regexp,
      'TaggedStringContentDouble TaggedStringContentSingle': t.regexp,
      'InterpolationStart InterpolationEnd': t.special(t.brace),
      LineComment: t.lineComment,
      BlockComment: t.blockComment,
      ArithOp: t.arithmeticOperator,
      BitOp: t.bitwiseOperator,
      LogicOp: t.logicOperator,
      CompareOp: t.compareOperator,
      UpdateOp: t.updateOperator,
      Equals: t.definitionOperator,
      Pipeline: t.controlOperator,
      '=>': t.punctuation,
      '( )': t.paren,
      '[ ]': t.squareBracket,
      '{ }': t.brace,
      '.': t.derefOperator,
      ', ;': t.separator,
      ':': t.punctuation,
    }),
    indentNodeProp.add({
      Block: (context) => context.column(context.node.from) + context.unit,
      ParamList: (context) => context.column(context.node.from) + context.unit,
      ArgList: (context) => context.column(context.node.from) + context.unit,
    }),
    foldNodeProp.add({
      'Block ObjectExpression ArrayExpression MatchExpression': foldInside,
    }),
  ],
});

import { LRLanguage } from '@codemirror/language';

export const tlangLanguage = LRLanguage.define({
  name: 'tlang',
  parser: parserWithMetadata,
  languageData: {
    commentTokens: { line: '//' },
  },
});

import { completeFromList } from '@codemirror/autocomplete';

export const tlangCompletion = tlangLanguage.data.of({
  autocomplete: completeFromList([
    { label: 'pub', type: 'keyword' },
    { label: 'fn', type: 'keyword' },
    { label: 'rec', type: 'keyword' },
    { label: 'return', type: 'keyword' },
    { label: 'if', type: 'keyword' },
    { label: 'else', type: 'keyword' },
    { label: 'let', type: 'keyword' },
    { label: 'match', type: 'keyword' },
    { label: 'loop', type: 'keyword' },
    { label: 'for', type: 'keyword' },
    { label: 'in', type: 'keyword' },
    { label: 'with', type: 'keyword' },
    { label: 'break', type: 'keyword' },
    { label: 'continue', type: 'keyword' },
    { label: 'enum', type: 'keyword' },
    { label: 'struct', type: 'keyword' },
    { label: 'protocol', type: 'keyword' },
    { label: 'impl', type: 'keyword' },
    { label: 'apply', type: 'keyword' },
    { label: 'not', type: 'keyword' },
    { label: 'and', type: 'keyword' },
    { label: 'or', type: 'keyword' },
    { label: 'self', type: 'keyword' },
    { label: 'true', type: 'keyword' },
    { label: 'false', type: 'keyword' },

    { label: 'Some', type: 'function' },
    { label: 'None', type: 'variable' },
    { label: 'Ok', type: 'function' },
    { label: 'Err', type: 'function' },
    { label: 'len', type: 'function' },
    { label: 'log', type: 'function' },
    { label: 'max', type: 'function' },
    { label: 'min', type: 'function' },
    { label: 'floor', type: 'function' },
    { label: 'random', type: 'function' },
    { label: 'random_int', type: 'function' },
    { label: 'compose', type: 'function' },
    { label: 'map', type: 'function' },
    { label: 'filter', type: 'function' },
    { label: 'filter_map', type: 'function' },
    { label: 'partition', type: 'function' },
    { label: 'foldl', type: 'function' },
    { label: 'foldr', type: 'function' },
    { label: 'sum', type: 'function' },
    { label: 'zip', type: 'function' },
  ]),
});

import { LanguageSupport } from '@codemirror/language';

/**
 * @param {{
 *   reLanguage?: import('@codemirror/language').Language,
 *   htmlLanguage?: import('@codemirror/language').Language,
 *   cssLanguage?: import('@codemirror/language').Language,
 *   sqlLanguage?: import('@codemirror/language').Language,
 *   jsonLanguage?: import('@codemirror/language').Language,
 *   jsLanguage?: import('@codemirror/language').Language,
 * }} languages
 */
function makeTaggedStringWrap(languages) {
  /** @type {Record<string, import('@lezer/common').Parser>} */
  const tagParsers = {};
  if (languages.reLanguage) tagParsers['re'] = languages.reLanguage.parser;
  if (languages.htmlLanguage)
    tagParsers['html'] = languages.htmlLanguage.parser;
  if (languages.cssLanguage) tagParsers['css'] = languages.cssLanguage.parser;
  if (languages.sqlLanguage) tagParsers['sql'] = languages.sqlLanguage.parser;
  if (languages.jsonLanguage)
    tagParsers['json'] = languages.jsonLanguage.parser;
  if (languages.jsLanguage) tagParsers['js'] = languages.jsLanguage.parser;

  if (Object.keys(tagParsers).length === 0) return undefined;

  return parseMixed((node, input) => {
    if (node.type.name !== 'TaggedString') return null;

    // Walk the TaggedString's children to find the tag name and content ranges.
    let tag = null;
    /** @type {Array<{from: number, to: number}>} */
    const overlay = [];

    const cursor = node.node.cursor();
    if (!cursor.firstChild()) return null;
    do {
      const name = cursor.type.name;
      if (name === 'TagStringTag') {
        tag = input.read(cursor.from, cursor.to);
      } else if (
        name === 'TaggedStringContentDouble' ||
        name === 'TaggedStringContentSingle'
      ) {
        overlay.push({ from: cursor.from, to: cursor.to });
      }
    } while (cursor.nextSibling());

    if (!tag || overlay.length === 0) return null;

    const parser = tagParsers[tag];
    if (!parser) return null;

    return { parser, overlay };
  });
}

/**
 * @param {{
 *   reLanguage?: import('@codemirror/language').Language,
 *   htmlLanguage?: import('@codemirror/language').Language,
 *   cssLanguage?: import('@codemirror/language').Language,
 *   sqlLanguage?: import('@codemirror/language').Language,
 *   jsonLanguage?: import('@codemirror/language').Language,
 *   jsLanguage?: import('@codemirror/language').Language,
 * }} [options]
 */
export function tlangLanguageSupport(options = {}) {
  const wrap = makeTaggedStringWrap(options);
  const lang = wrap ? tlangLanguage.configure({ wrap }) : tlangLanguage;
  return new LanguageSupport(lang, [tlangCompletion]);
}
