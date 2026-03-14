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

// V8 stack frame patterns:
//   "    at Name (file:line:col)"
//   "    at file:line:col"
const FRAME_WITH_FN = /^(\s+at )(.+?) \((.+?):(\d+):(\d+)\)(.*)$/;
const FRAME_NO_FN = /^(\s+at )(.+?):(\d+):(\d+)(.*)$/;

/**
 * Remap a V8 `error.stack` string using a source map.
 *
 * Frames referencing `sourceUrl` at a line > `prefixLines` are resolved
 * back to the original tlang source position.  All other frames are kept
 * as-is so the native Node / browser frames remain visible.
 */
export function remapStack(
  stack: string,
  map: ParsedSourceMap,
  sourceUrl: string,
  prefixLines: number,
): string {
  const sourceName = map.sources[0] ?? sourceUrl;
  return stack
    .split('\n')
    .map((line) => {
      // Try "at Name (file:line:col)" first, then "at file:line:col".
      const mFn = FRAME_WITH_FN.exec(line);
      const mNo = !mFn ? FRAME_NO_FN.exec(line) : null;

      if (!mFn && !mNo) return line;

      const [indent, fnName, file, rawLine, rawCol, suffix] = mFn
        ? [mFn[1], mFn[2], mFn[3], mFn[4], mFn[5], mFn[6]]
        : [mNo![1], '', mNo![2], mNo![3], mNo![4], mNo![5]];

      if (!file.includes(sourceUrl)) return line;

      const genLine = parseInt(rawLine, 10);
      const genCol = parseInt(rawCol, 10);

      // Lines inside the preamble (stdlib, bindings) have no tlang source.
      if (genLine <= prefixLines) return line;

      // Adjust for the preamble offset, then look up in the source map.
      const adjustedLine = genLine - prefixLines;
      const orig = originalPositionFor(map, adjustedLine, genCol);
      if (!orig) return line;

      const loc = `${sourceName}:${orig.line}:${orig.column}`;
      return fnName
        ? `${indent}${fnName} (${loc})${suffix}`
        : `${indent}${loc}${suffix}`;
    })
    .join('\n');
}
