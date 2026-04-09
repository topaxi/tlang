import {
  StateEffect,
  StateField,
  type Extension,
  type Text,
} from '@codemirror/state';
import {
  EditorView,
  ViewPlugin,
  type ViewUpdate,
  Decoration,
  type DecorationSet,
  WidgetType,
} from '@codemirror/view';

export interface InlayHint {
  /** UTF-16 code unit offset (CodeMirror document position). */
  position: number;
  /** The label text to display, e.g. `: i64` or `x:`. */
  label: string;
  /** Hint kind used for styling. Common values: `"type"`, `"parameter"`, `"returnType"`. */
  kind?: string;
}

export type InlayHintSource = (
  code: string,
) => InlayHint[] | Promise<InlayHint[]>;

export interface InlayHintsConfig {
  /** Debounce delay in milliseconds before recomputing hints. Default: `200`. */
  debounceMs?: number;
}

// ---------------------------------------------------------------------------
// Widget
// ---------------------------------------------------------------------------

class InlayHintWidget extends WidgetType {
  constructor(
    readonly label: string,
    readonly kind: string,
  ) {
    super();
  }

  override eq(other: InlayHintWidget) {
    return this.label === other.label && this.kind === other.kind;
  }

  override toDOM() {
    const span = document.createElement('span');
    span.className = `cm-inlay-hint cm-inlay-hint-${this.kind}`;
    span.textContent = this.label;
    span.setAttribute('aria-hidden', 'true');
    return span;
  }

  /** Never consume pointer or keyboard events. */
  override ignoreEvent() {
    return true;
  }
}

// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------

const setInlayHintsEffect = StateEffect.define<DecorationSet>();

const inlayHintsField = StateField.define<DecorationSet>({
  create: () => Decoration.none,
  update(decorations, tr) {
    // Map existing hint positions through any document changes so they stay
    // anchored to the right characters while the user types.
    decorations = decorations.map(tr.changes);
    for (const effect of tr.effects) {
      if (effect.is(setInlayHintsEffect)) {
        decorations = effect.value;
      }
    }
    return decorations;
  },
  provide: (field) => EditorView.decorations.from(field),
});

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function buildDecorations(hints: InlayHint[], doc: Text): DecorationSet {
  if (hints.length === 0) return Decoration.none;

  const ranges = hints.map((hint) => {
    const pos = Math.min(hint.position, doc.length);
    const kind = hint.kind ?? 'type';
    return Decoration.widget({
      widget: new InlayHintWidget(hint.label, kind),
      // side: 1 places the widget after the character at `pos`, so the hint
      // appears to the right of the token it annotates.
      side: 1,
    }).range(pos);
  });

  // Decoration.set requires ranges sorted by position; let CM sort them.
  return Decoration.set(ranges, true);
}

// ---------------------------------------------------------------------------
// Plugin
// ---------------------------------------------------------------------------

function makeInlayHintsPlugin(source: InlayHintSource, debounceMs: number) {
  return ViewPlugin.fromClass(
    class {
      private version = 0;
      private timeout: ReturnType<typeof setTimeout> | null = null;

      constructor(private readonly view: EditorView) {
        this.schedule();
      }

      update(update: ViewUpdate) {
        if (update.docChanged) this.schedule();
      }

      destroy() {
        if (this.timeout !== null) clearTimeout(this.timeout);
        // Bump version so any in-flight async call discards its result.
        this.version++;
      }

      private schedule() {
        if (this.timeout !== null) clearTimeout(this.timeout);
        this.timeout = setTimeout(() => {
          this.timeout = null;
          void this.run();
        }, debounceMs);
      }

      private async run() {
        const version = ++this.version;
        const code = this.view.state.doc.toString();

        let hints: InlayHint[];
        try {
          hints = await Promise.resolve(source(code));
        } catch {
          return;
        }

        // Discard stale results from a superseded run.
        if (version !== this.version) return;

        this.view.dispatch({
          effects: setInlayHintsEffect.of(
            buildDecorations(hints, this.view.state.doc),
          ),
        });
      }
    },
  );
}

// ---------------------------------------------------------------------------
// Theme
// ---------------------------------------------------------------------------

const inlayHintsBaseTheme = EditorView.baseTheme({
  '.cm-inlay-hint': {
    opacity: '0.6',
    fontSize: '0.85em',
    fontStyle: 'italic',
    pointerEvents: 'none',
    userSelect: 'none',
    padding: '0 1px',
  },
  // Parameter hints (e.g. `x:`) rendered slightly differently from type hints.
  '.cm-inlay-hint-parameter': {
    fontStyle: 'normal',
  },
});

// ---------------------------------------------------------------------------
// Public API
// ---------------------------------------------------------------------------

/**
 * CodeMirror 6 inlay hints extension.
 *
 * Accepts any `InlayHintSource` — a function that takes the current document
 * text and returns hints (sync or async).  Computation is debounced and
 * race-condition-safe: only the result of the most recent call is applied.
 *
 * @example
 * ```ts
 * EditorState.create({
 *   extensions: [
 *     inlayHints((code) => computeHints(code)),
 *   ],
 * });
 * ```
 */
export function inlayHints(
  source: InlayHintSource,
  config: InlayHintsConfig = {},
): Extension {
  const debounceMs = config.debounceMs ?? 200;
  return [
    inlayHintsField,
    makeInlayHintsPlugin(source, debounceMs),
    inlayHintsBaseTheme,
  ];
}
