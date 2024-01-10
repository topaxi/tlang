import { parser } from "./parser.js"
import { foldNodeProp, foldInside, indentNodeProp } from "@codemirror/language"
import { styleTags, tags as t } from "@lezer/highlight"

let parserWithMetadata = parser.configure({
  props: [
    styleTags({
      Identifier: t.variableName,
      Boolean: t.bool,
      String: t.string,
      LineComment: t.lineComment,
      "( )": t.paren
    }),
    indentNodeProp.add({
      Application: context => context.column(context.node.from) + context.unit
    }),
    foldNodeProp.add({
      Application: foldInside
    })
  ]
})

import { LRLanguage } from "@codemirror/language"

export const tlangLanguage = LRLanguage.define({
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

