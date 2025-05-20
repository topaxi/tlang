#!/usr/bin/env bash
set -euo pipefail

# Get absolute paths
PROJECT_ROOT="$(pwd)"
REPORTS_DIR="${PROJECT_ROOT}/reports"
CRATE_PATH="crates/tlang_runtime/tlang_interpreter"
CRATE_PATH_ABSOLUTE="${PROJECT_ROOT}/${CRATE_PATH}"
WORKTREE_DIR="${PROJECT_ROOT}/.benchmark-main-worktree"
WORKTREE_CRATE_PATH="${WORKTREE_DIR}/${CRATE_PATH}"
MAIN_BRANCH="origin/main"  # Use origin/main as default to ensure we have the latest
PR_BENCHMARK_FILE="pr_benchmark.txt"
MAIN_BENCHMARK_FILE="main_benchmark.txt"
PR_BENCHMARK_PATH="${REPORTS_DIR}/${PR_BENCHMARK_FILE}"
MAIN_BENCHMARK_PATH="${REPORTS_DIR}/${MAIN_BENCHMARK_FILE}"
CRITERION_DIR="target/criterion"
CRITERION_BACKUP_DIR="${PROJECT_ROOT}/${CRITERION_DIR}_main_backup"
BENCHMARK_FILTER="" # Optional filter to run specific benchmarks

# Function to display script usage
function show_usage {
  echo "Usage: $0 [options]"
  echo ""
  echo "Run benchmarks for both the current branch and main branch, then compare them."
  echo ""
  echo "Options:"
  echo "  -h, --help        Show this help message"
  echo "  --skip-main       Skip running benchmarks on main branch (use existing data)"
  echo "  --skip-pr         Skip running benchmarks on PR branch (use existing data)"
  echo "  --no-compare      Don't run comparison after benchmarks (just collect data)"
  echo "  --main-branch     Set main branch name (default: origin/main)"
  echo "  --filter NAME     Run only benchmarks containing NAME (e.g. 'fibonacci')"
  echo ""
  echo "This script will use git worktree to check out the main branch without"
  echo "disrupting your current work. Criterion will automatically compare against"
  echo "the previous run when benchmarks are executed sequentially."
}

# Parse command line arguments
SKIP_MAIN=0
SKIP_PR=0
RUN_COMPARE=1

while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)
      show_usage
      exit 0
      ;;
    --skip-main)
      SKIP_MAIN=1
      shift
      ;;
    --skip-pr)
      SKIP_PR=1
      shift
      ;;
    --no-compare)
      RUN_COMPARE=0
      shift
      ;;
    --main-branch)
      if [[ $# -lt 2 ]]; then
        echo "Error: --main-branch requires a branch name"
        exit 1
      fi
      MAIN_BRANCH="$2"
      shift 2
      ;;
    --filter)
      if [[ $# -lt 2 ]]; then
        echo "Error: --filter requires a benchmark name"
        exit 1
      fi
      BENCHMARK_FILTER="$2"
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      show_usage
      exit 1
      ;;
  esac
done

# Ensure we're in the project root directory
if [[ ! -d "crates" || ! -d ".git" ]]; then
  echo "Error: Please run this script from the project root directory."
  exit 1
fi

# Ensure cargo and git are available
if ! command -v cargo &> /dev/null; then
  echo "Error: cargo not found. Please install Rust toolchain."
  exit 1
fi

if ! command -v git &> /dev/null; then
  echo "Error: git not found. Please install git."
  exit 1
fi

# Ensure the crate directory exists
if [[ ! -d "$CRATE_PATH" ]]; then
  echo "Error: Benchmark crate path not found: $CRATE_PATH"
  exit 1
fi

# Create reports directory if it doesn't exist
mkdir -p "$REPORTS_DIR"

# Save current branch name for logging
CURRENT_BRANCH=$(git branch --show-current)
if [[ -z "$CURRENT_BRANCH" ]]; then
  CURRENT_BRANCH=$(git rev-parse HEAD)
  echo "Currently in detached HEAD state at $CURRENT_BRANCH"
else
  echo "Current branch: $CURRENT_BRANCH"
fi

# Make sure we have the latest from the remote
echo "Fetching latest main branch..."
git fetch origin main --quiet

# Benchmark the main branch using a worktree
if [[ $SKIP_MAIN -eq 0 ]]; then
  echo "=== Building and benchmarking main branch ==="
  
  # Clean up any existing worktree
  if [[ -d "$WORKTREE_DIR" ]]; then
    echo "Removing existing worktree..."
    git worktree remove -f "$WORKTREE_DIR" 2>/dev/null || true
  fi
  
  # Create a new worktree for main branch
  echo "Setting up worktree for $MAIN_BRANCH branch..."
  git worktree add "$WORKTREE_DIR" "$MAIN_BRANCH"
  
  # Run benchmarks in the worktree
  echo "Running benchmarks for main branch..."
  
  # Navigate to the crate directory in the worktree
  cd "${WORKTREE_CRATE_PATH}"
  
  # Run benchmarks with filter if provided
  if [[ -n "$BENCHMARK_FILTER" ]]; then
    echo "Using benchmark filter: $BENCHMARK_FILTER"
    cargo bench "$BENCHMARK_FILTER" | tee "${MAIN_BENCHMARK_PATH}"
  else
    cargo bench | tee "${MAIN_BENCHMARK_PATH}"
  fi
  
  # Return to project root
  cd "$PROJECT_ROOT"
  
  # Backup the Criterion data from the main branch
  if [[ -d "${WORKTREE_DIR}/${CRITERION_DIR}" ]]; then
    echo "Backing up Criterion data from main branch..."
    rm -rf "${CRITERION_BACKUP_DIR}" 2>/dev/null || true
    mkdir -p "$(dirname "${CRITERION_BACKUP_DIR}")"
    cp -r "${WORKTREE_DIR}/${CRITERION_DIR}" "${CRITERION_BACKUP_DIR}"
  fi
  
  # Clean up worktree
  cd ..
  echo "Cleaning up worktree..."
  git worktree remove -f "$WORKTREE_DIR"
  
  echo "Main branch benchmark results saved to $MAIN_BENCHMARK_FILE"
else
  echo "=== Skipping main branch benchmarks as requested ==="
  # Check if main benchmark file exists
  if [[ ! -f "$MAIN_BENCHMARK_FILE" ]]; then
    echo "Warning: Main benchmark file doesn't exist: $MAIN_BENCHMARK_FILE"
  else
    echo "Using existing main benchmark file: $MAIN_BENCHMARK_FILE"
  fi
fi

# Benchmark the PR branch (current branch)
if [[ $SKIP_PR -eq 0 ]]; then
  echo "=== Building and benchmarking current branch ($CURRENT_BRANCH) ==="
  
  # Before running PR benchmarks, clear any existing Criterion data to ensure
  # we're comparing against the main branch data we just backed up
  if [[ -d "${CRITERION_BACKUP_DIR}" ]]; then
    echo "Restoring Criterion data from main branch to ensure proper comparison..."
    rm -rf "${PROJECT_ROOT}/${CRITERION_DIR}" 2>/dev/null || true
    mkdir -p "$(dirname "${PROJECT_ROOT}/${CRITERION_DIR}")"
    cp -r "${CRITERION_BACKUP_DIR}" "${PROJECT_ROOT}/${CRITERION_DIR}"
  fi
  # Directory contents will be copied recursively (like `cp -r`).
  
  echo "Running benchmarks for current branch..."
  
  # Navigate to the crate directory
  cd "${CRATE_PATH_ABSOLUTE}"
  
  # Run benchmarks with filter if provided
  if [[ -n "$BENCHMARK_FILTER" ]]; then
    echo "Using benchmark filter: $BENCHMARK_FILTER"
    cargo bench "$BENCHMARK_FILTER" | tee "${PR_BENCHMARK_PATH}"
  else
    cargo bench | tee "${PR_BENCHMARK_PATH}" 
  fi
  
  # Return to project root
  cd "$PROJECT_ROOT"
  
  echo "PR branch benchmark results saved to $PR_BENCHMARK_PATH"
else
  echo "=== Skipping PR branch benchmarks as requested ==="
  # Check if PR benchmark file exists
  if [[ ! -f "$CRATE_PATH/$PR_BENCHMARK_FILE" ]]; then
    echo "Warning: PR benchmark file doesn't exist: $CRATE_PATH/$PR_BENCHMARK_FILE"
  else
    echo "Using existing PR benchmark file: $CRATE_PATH/$PR_BENCHMARK_FILE"
  fi
fi

# For GitHub Actions or if custom comparison is wanted, use our comparison script
if [[ $RUN_COMPARE -eq 1 ]]; then
  # Check if both benchmark files exist
  if [[ ! -f "${PR_BENCHMARK_PATH}" ]]; then
    echo "Warning: PR benchmark file not found: ${PR_BENCHMARK_PATH}"
  elif [[ ! -f "${MAIN_BENCHMARK_PATH}" ]]; then
    echo "Warning: Main benchmark file not found: ${MAIN_BENCHMARK_PATH}"
  else
    echo "=== Generating comparison report ==="
    # Ensure comparison script is executable
    if [[ -f "${PROJECT_ROOT}/scripts/compare_benchmarks.js" ]]; then
      chmod +x "${PROJECT_ROOT}/scripts/compare_benchmarks.js"
      echo "Running benchmark comparison script..."
      node "${PROJECT_ROOT}/scripts/compare_benchmarks.js" "${PR_BENCHMARK_PATH}" "${MAIN_BENCHMARK_PATH}"
    else
      echo "Warning: Benchmark comparison script not found: scripts/compare_benchmarks.js"
    fi
  fi
fi

echo "=== Benchmark process completed ==="
echo "Criterion automatically compares against the previous run, so check the output above for comparison results."
echo "You can also view Criterion's HTML report in '${PROJECT_ROOT}/${CRITERION_DIR}/report/index.html'"
echo "Benchmark results saved in: ${REPORTS_DIR}"