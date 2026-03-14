#!/usr/bin/env python3
"""Parse cargo nextest output diffs and update expected_output in test files."""

import re
import sys
from pathlib import Path


def parse_test_output(output_path: str) -> list[dict]:
    """Parse nextest output to extract failing test diffs."""
    with open(output_path) as f:
        content = f.read()

    results = []

    # Split on FAIL markers to isolate each test's output
    # Pattern: "FAIL [time] (N/M) package::module test_name"
    fail_pattern = re.compile(
        r'FAIL\s+\[.*?\]\s+\(\d+/\d+\)\s+\S+\s+(\w+)\s*\n'
        r'.*?panicked at (crates/\S+):(\d+):\d+:\s*\n'
        r'.*?Diff < left / right > :\n(.*?)(?=\n\s*note: run with|$)',
        re.DOTALL
    )

    for match in fail_pattern.finditer(content):
        test_name = match.group(1)
        file_path = match.group(2)
        line_no = int(match.group(3))
        diff_text = match.group(4)

        # Reconstruct "left" (actual) output from diff
        actual_lines = []
        for line in diff_text.split('\n'):
            if len(line) < 4:
                continue
            # Lines are indented by 4 spaces in the output
            stripped = line[4:] if line.startswith('    ') else line
            if not stripped:
                continue
            prefix = stripped[0]
            rest = stripped[1:] if len(stripped) > 1 else ''
            if prefix == '<':
                actual_lines.append(rest)
            elif prefix == '>':
                continue  # right-only line, skip for left reconstruction
            elif prefix == ' ':
                actual_lines.append(rest)

        actual_output = '\n'.join(actual_lines) + '\n'
        results.append({
            'test_name': test_name,
            'file_path': file_path,
            'assert_line': line_no,
            'actual_output': actual_output,
        })

    return results


def make_indoc_string(text: str, indent: int = 8) -> str:
    """Convert a string to indoc! format."""
    prefix = ' ' * indent
    close_prefix = ' ' * (indent - 4)
    lines = text.split('\n')
    # Remove trailing empty strings from split (but keep one trailing newline)
    while len(lines) > 1 and lines[-1] == '':
        lines.pop()

    indented = []
    for line in lines:
        if line:
            indented.append(prefix + line)
        else:
            indented.append('')

    inner = '\n'.join(indented)
    return f'indoc! {{\"\n{inner}\n{close_prefix}\"}}'


def make_inline_string(text: str) -> str:
    """Convert a string to an inline Rust string literal."""
    escaped = text.replace('\\', '\\\\').replace('"', '\\"').replace('\n', '\\n')
    return f'"{escaped}"'


def find_expected_output_range(lines: list[str], assert_line: int) -> tuple[int, int, int]:
    """Find the line range of the expected_output assignment before the assert_eq.

    Returns (start_line_idx, end_line_idx, indent_level).
    The range is inclusive of start, exclusive of end.
    """
    # assert_line is 1-indexed, convert to 0-indexed
    assert_idx = assert_line - 1

    # Search backward from the assert_eq line to find `let expected_output = `
    for i in range(assert_idx - 1, max(assert_idx - 30, -1), -1):
        line = lines[i]
        stripped = line.lstrip()
        if stripped.startswith('let expected_output ='):
            start_idx = i
            indent = len(line) - len(stripped)

            # Find the end of this statement
            # Could be single line: `let expected_output = "...";`
            # Or multi-line indoc: `let expected_output = indoc! {"...`
            #   continuing to `    "};`

            # Check if it ends on the same line
            if stripped.rstrip().endswith('";') or stripped.rstrip().endswith("';"):
                return (start_idx, start_idx + 1, indent)

            # Multi-line: find the closing `"};`
            for j in range(start_idx + 1, min(start_idx + 100, len(lines))):
                line_j = lines[j].rstrip()
                if line_j.endswith('"};') or line_j.endswith("'};"):
                    return (start_idx, j + 1, indent)

            # Fallback: couldn't find end
            return (start_idx, start_idx + 1, indent)

    return (-1, -1, 0)


def update_test_file(file_path: str, updates: list[dict]):
    """Apply updates to a test file. Updates should be sorted by line number descending."""
    with open(file_path) as f:
        lines = f.readlines()

    # Process from bottom to top to avoid line number shifts
    updates_sorted = sorted(updates, key=lambda u: u['assert_line'], reverse=True)

    for update in updates_sorted:
        assert_line = update['assert_line']
        actual = update['actual_output']

        start, end, indent = find_expected_output_range(lines, assert_line)
        if start == -1:
            print(f"  WARNING: Could not find expected_output for {update['test_name']} "
                  f"at line {assert_line}")
            continue

        # Determine format: inline or indoc
        if '\n' in actual.rstrip('\n'):
            # Multi-line: use indoc!
            new_value = make_indoc_string(actual, indent + 4)
        else:
            # Single line: use inline string
            new_value = make_inline_string(actual)

        indent_str = ' ' * indent
        new_line = f"{indent_str}let expected_output = {new_value};\n"

        # Replace lines[start:end] with the new assignment
        lines[start:end] = [new_line]

        print(f"  Updated {update['test_name']} (line {assert_line}): "
              f"replaced lines {start+1}-{end} with new expected_output")

    with open(file_path, 'w') as f:
        f.writelines(lines)


def main():
    output_path = sys.argv[1] if len(sys.argv) > 1 else '/tmp/test_output.txt'

    print(f"Parsing test output from {output_path}...")
    results = parse_test_output(output_path)
    print(f"Found {len(results)} failing tests")

    if not results:
        print("No failing tests found. Check the output format.")
        return

    # Group by file
    by_file: dict[str, list[dict]] = {}
    for r in results:
        by_file.setdefault(r['file_path'], []).append(r)

    for file_path, updates in by_file.items():
        print(f"\nUpdating {file_path} ({len(updates)} tests)...")
        update_test_file(file_path, updates)

    print(f"\nDone! Updated {len(results)} test expectations across {len(by_file)} files.")
    print("Run tests again to check for remaining failures (multi-assert tests may need another pass).")


if __name__ == '__main__':
    main()
