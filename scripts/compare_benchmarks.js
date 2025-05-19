const fs = require('fs');

/**
 * Standalone benchmark comparison script that can be run locally
 *
 * Usage:
 *   node compare_benchmarks.js <pr_benchmark_file> <main_benchmark_file>
 *
 * Example:
 *   node compare_benchmarks.js pr_benchmark.txt main_benchmark.txt
 */

// Get file paths from command line arguments
const args = process.argv.slice(2);
if (args.length !== 2) {
  console.error(
    'Usage: node compare_benchmarks.js <pr_benchmark_file> <main_benchmark_file>',
  );
  process.exit(1);
}

const prBenchmarkPath = args[0];
const mainBenchmarkPath = args[1];

// Function to safely read a file, returning empty string if it doesn't exist
function safeReadFile(filePath) {
  try {
    if (!fs.existsSync(filePath)) {
      console.error(`File does not exist: ${filePath}`);
      return '';
    }
    const content = fs.readFileSync(filePath, 'utf8');
    if (!content || content.trim() === '') {
      console.error(`File is empty: ${filePath}`);
      return '';
    }
    return content;
  } catch (error) {
    console.error(`Error reading file ${filePath}: ${error.message}`);
    return '';
  }
}

// Parse Criterion benchmark results from text output
function parseBenchmarkOutput(filePath) {
  const content = safeReadFile(filePath);
  if (!content) {
    console.warn(`No content found in ${filePath}`);
    return {};
  }

  const lines = content.split('\n');
  console.log(`Found ${lines.length} lines in ${filePath}`);

  const results = {};
  let currentGroup = null;

  for (const line of lines) {
    try {
      // Match benchmark group name
      const groupMatch = line.match(
        /^([a-zA-Z0-9_.-]+)\/([a-zA-Z0-9_() .-]+)$/,
      );
      if (groupMatch) {
        const groupName = groupMatch[1].trim();
        const benchName = groupMatch[2].trim();

        if (!results[groupName]) {
          results[groupName] = {};
        }

        if (!results[groupName][benchName]) {
          results[groupName][benchName] = {};
        }

        currentGroup = [groupName, benchName];
        continue;
      }

      // Match time
      const timeMatch = line.match(
        /time:\s+\[([\d.]+)\s\w+\s([\d.]+)\s\w+\s([\d.]+)\s\w+\]/,
      );
      if (timeMatch && currentGroup) {
        const [groupName, benchName] = currentGroup;

        // Parse the median and handle parsing errors
        let median;
        try {
          median = parseFloat(timeMatch[2]);
          if (isNaN(median)) {
            console.warn(
              `Invalid median value in ${filePath} for ${groupName}/${benchName}: ${timeMatch[2]}`,
            );
            continue;
          }
          median *= 1e9; // convert to ns
        } catch (e) {
          console.warn(`Error parsing median value: ${e.message}`);
          continue;
        }

        results[groupName][benchName] = { median };
      }
    } catch (lineError) {
      console.warn(`Error processing line in ${filePath}: ${line}`);
      console.warn(lineError);
      // Continue processing other lines
      continue;
    }
  }

  return results;
}

// Safe number formatter
function safeToFixed(value, digits = 2) {
  if (value === undefined || value === null || isNaN(value)) {
    return 'N/A';
  }
  return value.toFixed(digits);
}

try {
  console.log('Starting benchmark analysis...');

  // Parse the results from both branches
  console.log('Parsing PR benchmark results...');
  const prResults = parseBenchmarkOutput(prBenchmarkPath);
  console.log('PR benchmark groups found:', Object.keys(prResults).length);

  console.log('Parsing main benchmark results...');
  const mainResults = parseBenchmarkOutput(mainBenchmarkPath);
  console.log('Main benchmark groups found:', Object.keys(mainResults).length);

  // Generate markdown report
  let report = '## Benchmark Results\n\n';

  // Check if we have any results
  if (Object.keys(prResults).length === 0) {
    report += '⚠️ No benchmark results found for the PR branch.\n\n';
  }

  if (Object.keys(mainResults).length === 0) {
    report += '⚠️ No benchmark results found for the main branch.\n\n';
  }

  if (
    Object.keys(prResults).length === 0 ||
    Object.keys(mainResults).length === 0
  ) {
    report += 'No benchmarks found to compare.\n';
    console.log(report);
    process.exit(1);
  }

  // Process benchmark groups
  let totalBenchmarks = 0;
  let processedBenchmarks = 0;
  let improvements = 0;
  let regressions = 0;
  let neutral = 0;
  let errors = 0;

  for (const [group, benchmarks] of Object.entries(prResults)) {
    if (!mainResults[group]) {
      report += `### ${group}\n\n⚠️ No matching benchmark group found in main branch.\n\n`;
      continue;
    }

    report += `### ${group}\n\n`;
    report += '| Benchmark | PR (ns) | Main (ns) | Difference | Change |\n';
    report += '|-----------|---------|-----------|------------|--------|\n';

    for (const [benchmark, prResult] of Object.entries(benchmarks)) {
      totalBenchmarks++;

      if (!mainResults[group][benchmark]) {
        report += `| ${benchmark} | ${safeToFixed(prResult.median)} | N/A | N/A | ⚠️ No matching benchmark in main |\n`;
        errors++;
        continue;
      }

      const prMedian = prResult.median;
      const mainMedian = mainResults[group][benchmark].median;

      // Skip if either value is missing
      if (
        prMedian === undefined ||
        mainMedian === undefined ||
        prMedian === null ||
        mainMedian === null ||
        isNaN(prMedian) ||
        isNaN(mainMedian)
      ) {
        report += `| ${benchmark} | ${safeToFixed(prMedian)} | ${safeToFixed(mainMedian)} | N/A | ⚠️ Invalid data |\n`;
        errors++;
        continue;
      }

      processedBenchmarks++;

      const diff = prMedian - mainMedian;
      const percentChange = mainMedian !== 0 ? (diff / mainMedian) * 100 : 0;
      const isImprovement = diff < 0;

      if (isImprovement) {
        improvements++;
      } else if (Math.abs(percentChange) < 1) {
        neutral++;
      } else {
        regressions++;
      }

      const emoji = isImprovement
        ? '🟢'
        : Math.abs(percentChange) < 1
          ? '⚪'
          : '🔴';

      report += `| ${benchmark} | ${safeToFixed(prMedian)} | ${safeToFixed(mainMedian)} | ${safeToFixed(diff)} | ${emoji} ${safeToFixed(percentChange)}% |\n`;
    }

    report += '\n';
  }

  // Add summary
  report += `\n## Summary\n\n`;
  report += `- Total benchmarks: ${totalBenchmarks}\n`;
  report += `- Successfully compared: ${processedBenchmarks}\n`;
  report += `- Improvements: ${improvements} 🟢\n`;
  report += `- Neutral changes: ${neutral} ⚪\n`;
  report += `- Regressions: ${regressions} 🔴\n`;
  report += `- Errors: ${errors} ⚠️\n\n`;

  report +=
    '\n🟢 Improvement | ⚪ Neutral (< 1%) | 🔴 Regression | ⚠️ Error comparing\n';

  // Print the report to console
  console.log(report);
  console.log('\nBenchmark analysis completed successfully');
} catch (error) {
  console.error(`Error generating benchmark report: ${error.message}`);
  console.error(error.stack);
  process.exit(1);
}
