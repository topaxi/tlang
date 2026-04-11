import { parser } from './parser.js';
import {
  foldNodeProp,
  foldInside,
  indentNodeProp,
  LRLanguage,
  LanguageSupport,
} from '@codemirror/language';
import { styleTags, tags as t } from '@lezer/highlight';
import { parseMixed } from '@lezer/common';
import { completeFromList } from '@codemirror/autocomplete';
import { hoverTooltip, EditorView, ViewPlugin } from '@codemirror/view';

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
      TripleString: t.string,
      TagStringTag: t.special(t.regexp),
      'TaggedStringOpenDouble TaggedStringOpenSingle TaggedStringDoubleClose TaggedStringSingleClose TaggedStringOpenTripleDouble TaggedStringOpenTripleSingle TaggedStringTripleDoubleClose TaggedStringTripleSingleClose':
        t.regexp,
      'TaggedStringContentDouble TaggedStringContentSingle TaggedStringContentTripleDouble TaggedStringContentTripleSingle':
        t.regexp,
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

export const tlangLanguage = LRLanguage.define({
  name: 'tlang',
  parser: parserWithMetadata,
  languageData: {
    commentTokens: { line: '//' },
  },
});

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

/**
 * @typedef {{
 *   text: string,
 *   from: number,
 *   to: number,
 * }} HoverInfo
 *
 * @typedef {{
 *   from: number,
 *   to: number,
 * }} DefinitionLocation
 *
 * @typedef {(pos: number) => HoverInfo | null} HoverProvider
 * @typedef {(pos: number) => DefinitionLocation | null} GotoDefinitionProvider
 *
 * @typedef {{
 *   label: string,
 * }} SignatureParameterInformation
 *
 * @typedef {{
 *   label: string,
 *   parameters: SignatureParameterInformation[],
 * }} SignatureInformation
 *
 * @typedef {{
 *   signatures: SignatureInformation[],
 *   activeSignature: number,
 *   activeParameter: number,
 * }} SignatureHelp
 *
 * @typedef {(pos: number) => SignatureHelp | null | Promise<SignatureHelp | null>} SignatureHelpProvider
 */

/**
 * @param {{
 *   reLanguage?: import('@codemirror/language').Language,
 *   htmlLanguage?: import('@codemirror/language').Language,
 *   cssLanguage?: import('@codemirror/language').Language,
 *   sqlLanguage?: import('@codemirror/language').Language,
 *   jsonLanguage?: import('@codemirror/language').Language,
 *   jsLanguage?: import('@codemirror/language').Language,
 *   markdownLanguage?: import('@codemirror/language').Language,
 *   hoverProvider?: HoverProvider,
 *   gotoDefinitionProvider?: GotoDefinitionProvider,
 *   signatureHelpProvider?: SignatureHelpProvider,
 * }} [options]
 */
export function tlangLanguageSupport(options = {}) {
  const wrap = makeTaggedStringWrap(options);
  const lang = wrap ? tlangLanguage.configure({ wrap }) : tlangLanguage;

  /** @type {import('@codemirror/state').Extension[]} */
  const extensions = [tlangCompletion];

  if (options.hoverProvider) {
    extensions.push(tlangHoverTooltip(options.hoverProvider));
  }

  if (options.gotoDefinitionProvider) {
    extensions.push(tlangGotoDefinition(options.gotoDefinitionProvider));
  }

  if (options.signatureHelpProvider) {
    extensions.push(tlangSignatureHelp(options.signatureHelpProvider));
  }

  return new LanguageSupport(lang, extensions);
}

/**
 * Create a CodeMirror hover tooltip extension using the given provider.
 *
 * @param {HoverProvider} provider
 */
function tlangHoverTooltip(provider) {
  return hoverTooltip((_view, pos, _side) => {
    const info = provider(pos);
    if (!info) return null;

    return {
      pos: info.from,
      end: info.to,
      create: () => {
        const dom = document.createElement('div');
        dom.className = 'cm-tlang-hover';
        dom.textContent = info.text;
        return { dom };
      },
    };
  });
}

/**
 * Create a CodeMirror goto-definition extension (Ctrl/Cmd+Click).
 *
 * @param {GotoDefinitionProvider} provider
 */
function tlangGotoDefinition(provider) {
  return EditorView.domEventHandlers({
    click: (event, view) => {
      if (!event.ctrlKey && !event.metaKey) return false;

      const pos = view.posAtCoords({ x: event.clientX, y: event.clientY });
      if (pos === null || pos === undefined) return false;

      const def = provider(pos);
      if (!def) return false;

      event.preventDefault();
      view.dispatch({
        selection: { anchor: def.from },
        scrollIntoView: true,
      });
      return true;
    },
  });
}

const signatureHelpTheme = EditorView.baseTheme({
  '.cm-editor': {
    position: 'relative',
  },
  '.cm-tooltip.cm-tooltip-tlang-signature-help': {
    maxWidth: 'min(36rem, calc(100vw - 2rem))',
  },
  '.cm-tlang-signature-help': {
    fontFamily: 'var(--t-font-family-mono, monospace)',
    fontSize: '0.85em',
    padding: '4px 8px',
  },
  '.cm-tlang-signature-help-overload': {
    opacity: '0.7',
    marginBottom: '2px',
    fontSize: '0.9em',
  },
  '.cm-tlang-signature-help-param-active': {
    fontWeight: '700',
    textDecoration: 'underline',
  },
});

/**
 * Create a CodeMirror signature-help tooltip extension using the given provider.
 *
 * @param {SignatureHelpProvider} provider
 */
export function tlangSignatureHelp(provider) {
  return [
    signatureHelpTheme,
    ViewPlugin.fromClass(
      class {
        /** @type {ReturnType<typeof setTimeout> | null} */
        timeout = null;
        version = 0;

        /**
         * @param {EditorView} view
         */
        constructor(view) {
          this.view = view;
          this.dom = document.createElement('div');
          this.dom.className = 'cm-tooltip cm-tooltip-tlang-signature-help';
          this.dom.style.position = 'absolute';
          this.dom.style.display = 'none';
          this.dom.setAttribute('role', 'tooltip');
          this.view.dom.append(this.dom);

          this.handleGeometryChange = () => this.schedule();
          this.view.scrollDOM.addEventListener(
            'scroll',
            this.handleGeometryChange,
            {
              passive: true,
            },
          );
          window.addEventListener('resize', this.handleGeometryChange);

          this.schedule();
        }

        /**
         * @param {import('@codemirror/view').ViewUpdate} update
         */
        update(update) {
          if (
            update.docChanged ||
            update.selectionSet ||
            update.focusChanged ||
            update.geometryChanged ||
            update.viewportChanged
          ) {
            this.schedule();
          }
        }

        destroy() {
          if (this.timeout !== null) clearTimeout(this.timeout);
          this.version++;
          this.view.scrollDOM.removeEventListener(
            'scroll',
            this.handleGeometryChange,
          );
          window.removeEventListener('resize', this.handleGeometryChange);
          this.dom.remove();
        }

        schedule() {
          if (this.timeout !== null) clearTimeout(this.timeout);
          this.timeout = setTimeout(() => {
            this.timeout = null;
            void this.run();
          }, 75);
        }

        async run() {
          const version = ++this.version;
          const selection = this.view.state.selection.main;

          if (!this.view.hasFocus || !selection.empty) {
            this.clear();
            return;
          }

          const pos = selection.head;
          const help = await Promise.resolve(provider(pos));

          if (version !== this.version) return;

          if (!help || help.signatures.length === 0) {
            this.clear();
            return;
          }

          const activeSignature = clampIndex(
            help.activeSignature,
            help.signatures.length,
          );
          const signature = help.signatures[activeSignature];
          const activeParameter = clampIndex(
            help.activeParameter,
            signature.parameters.length,
          );

          this.dom.replaceChildren(
            renderSignatureHelpDom(
              help.signatures,
              activeSignature,
              activeParameter,
            ),
          );
          this.positionTooltip(pos);
        }

        clear() {
          this.dom.style.display = 'none';
        }

        positionTooltip(pos) {
          const coords = this.view.coordsAtPos(pos);
          if (!coords) {
            this.clear();
            return;
          }

          const editorRect = this.view.dom.getBoundingClientRect();
          this.dom.style.display = 'block';
          this.dom.style.visibility = 'hidden';

          const maxLeft = Math.max(
            0,
            this.view.dom.clientWidth - this.dom.offsetWidth - 8,
          );
          const left = Math.max(
            0,
            Math.min(coords.left - editorRect.left, maxLeft),
          );

          let top = coords.top - editorRect.top - this.dom.offsetHeight - 8;
          if (top < 0) {
            top = coords.bottom - editorRect.top + 8;
          }

          this.dom.style.left = `${left}px`;
          this.dom.style.top = `${top}px`;
          this.dom.style.visibility = 'visible';
        }
      },
    ),
  ];
}

/**
 * @param {number} index
 * @param {number} length
 */
function clampIndex(index, length) {
  if (length === 0) return 0;
  return Math.max(0, Math.min(index, length - 1));
}

/**
 * @param {SignatureInformation[]} signatures
 * @param {number} activeSignature
 * @param {number} activeParameter
 */
function renderSignatureHelpDom(signatures, activeSignature, activeParameter) {
  const container = document.createElement('div');
  container.className = 'cm-tlang-signature-help';

  if (signatures.length > 1) {
    const overload = document.createElement('div');
    overload.className = 'cm-tlang-signature-help-overload';
    overload.textContent = `${activeSignature + 1} of ${signatures.length}`;
    container.append(overload);
  }

  const label = document.createElement('div');
  label.className = 'cm-tlang-signature-help-label';
  appendSignatureLabel(label, signatures[activeSignature], activeParameter);
  container.append(label);

  return container;
}

/**
 * @param {HTMLElement} target
 * @param {SignatureInformation} signature
 * @param {number} activeParameter
 */
function appendSignatureLabel(target, signature, activeParameter) {
  if (signature.parameters.length === 0) {
    target.textContent = signature.label;
    return;
  }

  let searchFrom = 0;

  for (const [index, parameter] of signature.parameters.entries()) {
    const paramIndex = signature.label.indexOf(parameter.label, searchFrom);

    if (paramIndex === -1) {
      target.textContent = signature.label;
      return;
    }

    target.append(
      document.createTextNode(signature.label.slice(searchFrom, paramIndex)),
    );

    const span = document.createElement('span');
    span.textContent = parameter.label;

    if (index === activeParameter) {
      span.className = 'cm-tlang-signature-help-param-active';
    }

    target.append(span);
    searchFrom = paramIndex + parameter.label.length;
  }

  target.append(document.createTextNode(signature.label.slice(searchFrom)));
}

/**
 * @param {{
 *   reLanguage?: import('@codemirror/language').Language,
 *   htmlLanguage?: import('@codemirror/language').Language,
 *   cssLanguage?: import('@codemirror/language').Language,
 *   sqlLanguage?: import('@codemirror/language').Language,
 *   jsonLanguage?: import('@codemirror/language').Language,
 *   jsLanguage?: import('@codemirror/language').Language,
 *   markdownLanguage?: import('@codemirror/language').Language,
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
  if (languages.markdownLanguage)
    tagParsers['md'] = tagParsers['markdown'] =
      languages.markdownLanguage.parser;

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
        name === 'TaggedStringContentSingle' ||
        name === 'TaggedStringContentTripleDouble' ||
        name === 'TaggedStringContentTripleSingle'
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
