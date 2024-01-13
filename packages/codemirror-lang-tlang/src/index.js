import { parser } from "./parser.js"
import { foldNodeProp, foldInside, indentNodeProp } from "@codemirror/language"
import { styleTags, tags as t } from "@lezer/highlight"

let parserWithMetadata = parser.configure({
  props: [
    styleTags({
      VariableName: t.variableName,
      VariableDefinition: t.definition(t.variableName),
      "CallExpression/VariableName": t.function(t.variableName),
      "if else return rec": t.controlKeyword,
      "let enum fn": t.definitionKeyword,
      "FunctionDeclaration/VariableDefinition": t.function(t.definition(t.variableName)),
      PropertyName: t.propertyName,
      "CallExpression/MemberExpression/PropertyName": t.function(t.propertyName),
      BooleanLiteral: t.bool,
      Number: t.number,
      String: t.string,
      LineComment: t.lineComment,
      BlockComment: t.blockComment,
      ArithOp: t.arithmeticOperator,
      CompareOp: t.compareOperator,
      Equals: t.definitionOperator,
      Pipeline: t.controlOperator,
      "( )": t.paren,
      "[ ]": t.squareBracket,
      "{ }": t.brace,
      ".": t.derefOperator,
      ", ;": t.separator,
      "_": t.special,
    }),
    indentNodeProp.add({
      Application: context => context.column(context.node.from) + context.unit
    }),
    foldNodeProp.add({
      "Block ObjectExpression ArrayExpression": foldInside
    })
  ]
})

import { LRLanguage } from "@codemirror/language"

export const tlangLanguage = LRLanguage.define({
  name: 'tlang',
  parser: parserWithMetadata,
  languageData: {
    commentTokens: { line: ";" }
  }
})

import { completeFromList } from "@codemirror/autocomplete"

export const tlangCompletion = tlangLanguage.data.of({
  autocomplete: completeFromList([
    { label: "fn", type: "keyword" },
    { label: "rec", type: "keyword" },
    { label: "return", type: "keyword" },
    { label: "if", type: "keyword" },
    { label: "let", type: "keyword" },
    { label: "else", type: "keyword" },
    { label: "enum", type: "keyword" },

    { label: "log", type: "function" },
    { label: "max", type: "function" },
    { label: "min", type: "function" },
    { label: "floor", type: "function" },
    { label: "random", type: "function" },
  ])
})


import { LanguageSupport } from "@codemirror/language"

export function tlangLanguageSupport() {
  return new LanguageSupport(tlangLanguage, [tlangCompletion]);
}

