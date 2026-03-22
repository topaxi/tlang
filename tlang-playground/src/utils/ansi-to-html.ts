/**
 * Minimal ANSI escape sequence → HTML converter for codespan-reporting output.
 *
 * codespan-reporting uses a small subset of ANSI SGR codes:
 *   - 1  bold
 *   - 2  dimmed
 *   - 4  underline
 *   - 22 bold-off
 *   - 24 underline-off
 *   - 38;5;<n> / 38;2;<r>;<g>;<b>  foreground color
 *   - 39 default foreground
 *   - 0  reset
 *
 * Colors are mapped to Catppuccin Macchiato CSS custom properties so they
 * harmonise with the rest of the playground UI.
 */

// Sentinel wrapper so the console component can detect pre-rendered HTML.
export class DiagnosticHtml {
  constructor(public readonly html: string) {}
}

// codespan-reporting 256-color palette indices it actually emits for Macchiato-
// compatible terminals.  We only need a handful: the library uses the standard
// 16 ANSI colors plus a few fixed indices for its own palette.
//
// Mapping: ANSI 256-color index → CSS custom property name (--ctp-macchiato-*)
const ANSI_256_TO_CSS: Record<number, string> = {
  // Standard 16 colors remapped to Catppuccin Macchiato
  1: '--ctp-macchiato-red', // dark red
  2: '--ctp-macchiato-green', // dark green
  3: '--ctp-macchiato-yellow', // dark yellow
  4: '--ctp-macchiato-blue', // dark blue
  5: '--ctp-macchiato-mauve', // dark magenta
  6: '--ctp-macchiato-teal', // dark cyan
  7: '--ctp-macchiato-text', // light gray
  9: '--ctp-macchiato-red', // bright red
  10: '--ctp-macchiato-green', // bright green
  11: '--ctp-macchiato-yellow', // bright yellow
  12: '--ctp-macchiato-blue', // bright blue
  13: '--ctp-macchiato-mauve', // bright magenta
  14: '--ctp-macchiato-sky', // bright cyan
  15: '--ctp-macchiato-text', // white
};

interface State {
  bold: boolean;
  dim: boolean;
  underline: boolean;
  fg: string | null; // CSS color value or var(...)
}

function stateToOpenTag(state: State): string {
  const styles: string[] = [];

  if (state.bold) styles.push('font-weight:bold');
  if (state.dim) styles.push('opacity:0.65');
  if (state.underline) styles.push('text-decoration:underline');
  if (state.fg) styles.push(`color:${state.fg}`);

  if (styles.length === 0) return '';
  return `<span style="${styles.join(';')}">`;
}

function escapeHtml(text: string): string {
  return text
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

function resolveFg(params: number[]): string | null {
  if (params[0] === 38 && params[1] === 5 && params[2] !== undefined) {
    // 256-color: 38;5;<n>
    const css = ANSI_256_TO_CSS[params[2]];
    return css ? `var(${css})` : null;
  }
  if (
    params[0] === 38 &&
    params[1] === 2 &&
    params[2] !== undefined &&
    params[3] !== undefined &&
    params[4] !== undefined
  ) {
    // True-color: 38;2;<r>;<g>;<b>
    return `rgb(${params[2]},${params[3]},${params[4]})`;
  }
  return null;
}

/**
 * Convert an ANSI-colored string (as emitted by codespan-reporting) to an
 * HTML string with inline `<span style="...">` elements.
 */
export function ansiToHtml(input: string): string {
  // ESC [ ... m  — SGR sequences only
  const ESC = String.fromCharCode(27);
  const re = new RegExp(`${ESC}\\[([0-9;]*)m`, 'g');

  let result = '';
  let lastIndex = 0;
  let state: State = { bold: false, dim: false, underline: false, fg: null };
  let openCount = 0; // how many <span> tags are currently open

  const applyState = (newState: State) => {
    // Close all open spans first
    result += '</span>'.repeat(openCount);
    openCount = 0;
    state = newState;
    const tag = stateToOpenTag(state);
    if (tag) {
      result += tag;
      openCount = 1;
    }
  };

  let match: RegExpExecArray | null;
  while ((match = re.exec(input)) !== null) {
    // Flush plain text before this escape sequence
    result += escapeHtml(input.slice(lastIndex, match.index));
    lastIndex = re.lastIndex;

    const raw = match[1];
    const params = raw === '' ? [0] : raw.split(';').map(Number);

    const newState: State = { ...state };
    let i = 0;
    while (i < params.length) {
      const code = params[i];
      switch (code) {
        case 0:
          newState.bold = false;
          newState.dim = false;
          newState.underline = false;
          newState.fg = null;
          break;
        case 1:
          newState.bold = true;
          break;
        case 2:
          newState.dim = true;
          break;
        case 4:
          newState.underline = true;
          break;
        case 22:
          newState.bold = false;
          newState.dim = false;
          break;
        case 24:
          newState.underline = false;
          break;
        case 39:
          newState.fg = null;
          break;
        case 38: {
          const sub = params.slice(i);
          const color = resolveFg(sub);
          if (color) {
            newState.fg = color;
            // Consume the extra params (38;5;n → +2, 38;2;r;g;b → +4)
            i += sub[1] === 5 ? 2 : 4;
          }
          break;
        }
        default:
          // Standard 16-color foreground (30-37, 90-97)
          if ((code >= 30 && code <= 37) || (code >= 90 && code <= 97)) {
            const idx = code >= 90 ? code - 90 + 8 : code - 30;
            const css = ANSI_256_TO_CSS[idx + 1]; // offset: we skip 0=black
            newState.fg = css ? `var(${css})` : null;
          }
      }
      i++;
    }
    applyState(newState);
  }

  // Flush remaining plain text
  result += escapeHtml(input.slice(lastIndex));
  result += '</span>'.repeat(openCount);

  return result;
}
