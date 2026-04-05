#!/usr/bin/env python3
import subprocess
import platform
import os
import re
import json
import argparse
from datetime import datetime
from pathlib import Path

# This script coordinates benchmarking (via Mix) and report generation.
# It stores persistent results in JSON to allow targeted updates.

REPO_DIR = Path(__file__).resolve().parent.parent
JSON_FILE = REPO_DIR / "priv/data/benchmark.json"
BENCHMARK_FILE = REPO_DIR / "BENCHMARKS.md"
README_FILE = REPO_DIR / "README.md"

DIFF_ICON = {
    "xs": "🟢",
    "s":  "🟡",
    "m":  "🟠",
    "l":  "🔴",
    "xl": "💀",
}

def get_cpu_info():
    try:
        # Detailed CPU info on Linux
        output = subprocess.check_output(["lscpu"], text=True)
        for line in output.splitlines():
            if "Model name" in line:
                return line.split(":", 1)[1].strip()
    except Exception:
        pass
    return platform.processor() or "Unknown CPU"

def generate_markdown():
    if not JSON_FILE.exists():
        print("No benchmark data found. Run a benchmark first.")
        return

    with open(JSON_FILE) as f:
        data = json.load(f)

    system_info = {
        "os": platform.system(),
        "release": platform.release(),
        "cpu": get_cpu_info()
    }
    
    now = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    
    lines = [
        "# ⚡ Benchmarks\n\n",
        "Performance results for all solved Advent of Code challenges.\n\n",
        "## System Information\n\n",
        f"- **Run Date:** {now}\n",
        f"- **OS:** {system_info['os']} {system_info['release']}\n",
        f"- **CPU:** {system_info['cpu']}\n\n",
        "## Performance Results\n\n",
    ]
    
    # Sort years descending
    years = sorted(data.keys(), reverse=True)
    total_all_ms = 0

    for year in years:
        lines.append(f"### {year}\n\n")
        lines.append("| Day | Title | Difficulty | Time (ms) |\n")
        lines.append("|:---:|-------|:----------:|----------:|\n")
        
        year_data = data[year]
        # Sort days ascending
        days = sorted(year_data.keys(), key=int)
        year_total = 0
        
        for day in days:
            res = year_data[day]
            icon = DIFF_ICON.get(res["difficulty"], "⚪")
            lines.append(f"| {res['day']} | [{res['title']}]({res['link']}) | {icon} | {res['time_ms']:.2f} |\n")
            year_total += res["time_ms"]
        
        lines.append(f"| **Total** | | | **{year_total:.2f}** |\n\n")
        total_all_ms += year_total

    lines.append(f"**Total performance across all years: {total_all_ms/1000:.2f}s**\n")

    BENCHMARK_FILE.write_text("".join(lines))
    print(f"✨ Updated report: {BENCHMARK_FILE}")

def update_readme():
    if not README_FILE.exists():
        return
    
    text = README_FILE.read_text()
    if "BENCHMARKS.md" in text:
        return
    
    # Add link to the stats block legend
    pattern = r'(\[Difficulty\]\(wiki/difficulty\.md\))'
    if re.search(pattern, text):
        new_text = re.sub(pattern, r'\1 · [Benchmarks](BENCHMARKS.md)', text)
        README_FILE.write_text(new_text)
        print("Patched README.md with benchmark link.")

def main():
    parser = argparse.ArgumentParser(description="AoC Benchmark Runner")
    parser.add_argument("year", nargs="?", help="Year to benchmark")
    parser.add_argument("day", nargs="?", help="Day to benchmark")
    args = parser.parse_args()

    # Step 1: Run benchmarks
    mix_cmd = ["mix", "benchmark"]
    if args.year:
        mix_cmd.append(args.year)
        if args.day:
            mix_cmd.append(args.day)

    print(f"🚀 Running: {' '.join(mix_cmd)}")
    result = subprocess.run(mix_cmd, cwd=REPO_DIR)
    
    if result.returncode != 0:
        print("❌ Benchmark failed.")
        return

    # Step 2: Generate Report
    generate_markdown()
    
    # Step 3: Patch README
    update_readme()

if __name__ == "__main__":
    main()
