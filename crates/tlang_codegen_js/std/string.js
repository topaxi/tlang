import { string } from './string.tlang.js';
import { $Display } from './protocols.tlang.js';

string ??= {};

/**
 * @param {string} str
 * @param {number} idx
 */
string.char_code_at = (str, idx) => str.charCodeAt(idx);

/**
 * Tagged string interpolation: `f"text {expr} more"` → `f(["text ", " more"], [expr])`
 *
 * Each interpolated value is converted via Display::to_string.
 * @param {string[]} parts
 * @param {unknown[]} values
 * @returns {string}
 */
export function f(parts, values) {
  let result = parts[0];
  for (let i = 0; i < values.length; i++) {
    result += $Display.to_string(values[i]) + parts[i + 1];
  }
  return result;
}
