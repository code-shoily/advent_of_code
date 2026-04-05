#!/usr/bin/env python3
import subprocess
import os
from pathlib import Path

# This script is a wrapper around 'mix benchmark' to keep the workflow
# similar to the Gleam repository while leveraging Elixir's performance.

REPO_DIR = Path(__file__).resolve().parent.parent

def main():
    print("Running Elixir benchmarks... This might take a minute.")
    # We call our new mix task which handles benchmarking and file generation
    try:
        result = subprocess.run(
            ["mix", "benchmark"],
            cwd=REPO_DIR,
            check=True
        )
        print("\n✨ Benchmark generation complete!")
        print(f"📄 Report written to: {REPO_DIR / 'BENCHMARKS.md'}")
    except subprocess.CalledProcessError:
        print("\n❌ Error running mix benchmark.")
    except FileNotFoundError:
        print("\n❌ 'mix' command not found. (Elixir is required)")

if __name__ == "__main__":
    main()
