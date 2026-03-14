/**
 * Minimal source map consumer for remapping V8 stack traces in the playground.
 *
 * Handles Source Map Revision 3 (https://tc39.es/source-map/).
 * Only single-source maps are needed here (one tlang file → one JS output).
 */

const VLQ_CHARS =
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
const VLQ_MAP = new Map<string, number>([...VLQ_CHARS].map((c, i) => [c, i]));

function decodeVlqField(str: string, pos: number): [number, number] {
  let result = 0;
  let shift = 0;
  let cont: boolean;
  do {
    const digit = VLQ_MAP.get(str[pos++])!;
    cont = (digit & 0x20) !== 0;
    result |= (digit & 0x1f) << shift;
    shift += 5;
  } while (cont);
  return [result & 1 ? -(result >> 1) : result >> 1, pos];
}

interface Mapping {
  generatedLine: number; // 0-based
  generatedColumn: number; // 0-based
  sourceLine: number; // 0-based
  sourceColumn: number; // 0-based
}

export interface ParsedSourceMap {
  sources: string[];
  mappings: Mapping[];
}

export function parseSourceMap(mapJson: string): ParsedSourceMap {
  const raw = JSON.parse(mapJson) as {
    mappings: string;
    sources: string[];
  };
  const mappings: Mapping[] = [];
  const lines = raw.mappings.split(';');

  let srcLine = 0;
  let srcCol = 0;

  for (let genLine = 0; genLine < lines.length; genLine++) {
    const line = lines[genLine];
    if (!line) continue;

    let genCol = 0;
    for (const seg of line.split(',')) {
      if (!seg) continue;
      let pos = 0;
      let delta: number;

      [delta, pos] = decodeVlqField(seg, pos);
      genCol += delta;

      // Segments with only one field have no source info.
      if (pos >= seg.length) continue;

      [, pos] = decodeVlqField(seg, pos); // source index (ignored — single source)
      [delta, pos] = decodeVlqField(seg, pos);
      srcLine += delta;
      [delta] = decodeVlqField(seg, pos);
      srcCol += delta;

      mappings.push({
        generatedLine: genLine,
        generatedColumn: genCol,
        sourceLine: srcLine,
        sourceColumn: srcCol,
      });
    }
  }

  return { sources: raw.sources ?? [], mappings };
}

/** Find the original (1-based) position for a generated (1-based) location. */
export function originalPositionFor(
  map: ParsedSourceMap,
  genLine: number,
  genCol: number,
): { line: number; column: number } | null {
  // Convert to 0-based for map lookup.
  const gl = genLine - 1;
  const gc = genCol - 1;

  let best: Mapping | null = null;
  for (const m of map.mappings) {
    if (m.generatedLine > gl) break;
    if (m.generatedLine === gl && m.generatedColumn <= gc) {
      if (
        !best ||
        m.generatedLine > best.generatedLine ||
        (m.generatedLine === best.generatedLine &&
          m.generatedColumn > best.generatedColumn)
      ) {
        best = m;
      }
    }
  }

  return best
    ? { line: best.sourceLine + 1, column: best.sourceColumn + 1 }
    : null;
}

/**
 * Return `mapJson` with `offset` empty lines prepended to the VLQ mappings.
 *
 * Source map lines are separated by `;`, so prepending N semicolons shifts
 * every generated-line reference forward by N lines — exactly what is needed
 * to account for preamble code (stdlib, bindings destructuring) that precedes
 * the user's compiled code inside `new Function(…)`.
 */
export function shiftSourceMapLines(mapJson: string, offset: number): string {
  if (offset === 0) return mapJson;
  const prefix = ';'.repeat(offset);
  return mapJson.replace(/"mappings":"/, `"mappings":"${prefix}`);
}

// ---------------------------------------------------------------------------
// Stack frame parsing — handles Chrome/V8 and Firefox/Safari formats.
// ---------------------------------------------------------------------------

interface StackFrame {
  fnName: string;
  file: string;
  line: string;
  col: string;
}

// Chrome/V8:  "    at Name (file:line:col)"  or  "    at file:line:col"
const CHROME_WITH_FN = /^\s+at (.+?) \((.+?):(\d+):(\d+)\).*$/;
const CHROME_NO_FN = /^\s+at (.+?):(\d+):(\d+).*$/;

function parseFrame(raw: string): StackFrame | null {
  // Chrome/V8 with function name
  const mFn = CHROME_WITH_FN.exec(raw);
  if (mFn) return { fnName: mFn[1], file: mFn[2], line: mFn[3], col: mFn[4] };

  // Chrome/V8 without function name
  const mNo = CHROME_NO_FN.exec(raw);
  if (mNo) return { fnName: '', file: mNo[1], line: mNo[2], col: mNo[3] };

  // Firefox/Safari: "Name@file:line:col"  (Name may be empty or complex)
  // Use lastIndexOf('@') so closure names with '@' are handled correctly.
  const atIdx = raw.lastIndexOf('@');
  if (atIdx < 0) return null;

  const fnName = raw.slice(0, atIdx);
  const rest = raw.slice(atIdx + 1);

  // Pull :col and :line off the right-hand end.
  const col2 = rest.lastIndexOf(':');
  if (col2 < 0) return null;
  const col1 = rest.lastIndexOf(':', col2 - 1);
  if (col1 < 0) return null;

  const file = rest.slice(0, col1);
  const line = rest.slice(col1 + 1, col2);
  const col = rest.slice(col2 + 1);

  if (!line || !col || isNaN(parseInt(line, 10))) return null;
  return { fnName, file, line, col };
}

/**
 * Remap an `error.stack` string using a source map.
 *
 * Both Chrome/V8 (`at name (file:line:col)`) and Firefox/Safari
 * (`name@file:line:col`) stack formats are detected and normalised to
 * Chrome format for consistent display in the playground console.
 *
 * `prefixLines` (default 0) can be set when a preamble (e.g. stdlib code)
 * precedes the user code — frames at or below that line number are left
 * unchanged. When using a module-based executor with no preamble, omit this.
 */
export function remapStack(
  stack: string,
  map: ParsedSourceMap,
  sourceUrl: string,
  prefixLines = 0,
): string {
  const sourceName = map.sources[0] ?? sourceUrl;
  return stack
    .split('\n')
    .map((raw) => {
      const frame = parseFrame(raw);
      if (!frame || !frame.file.includes(sourceUrl)) return raw;

      const genLine = parseInt(frame.line, 10);
      const genCol = parseInt(frame.col, 10);

      // Preamble frames (stdlib, bindings) have no tlang source — keep raw.
      if (genLine <= prefixLines) return raw;

      // The map is already shifted by prefixLines — look up genLine directly,
      // no additional adjustment needed.
      const orig = originalPositionFor(map, genLine, genCol);
      if (!orig) return raw;

      const loc = `${sourceName}:${orig.line}:${orig.column}`;
      return frame.fnName ? `    at ${frame.fnName} (${loc})` : `    at ${loc}`;
    })
    .join('\n');
}
