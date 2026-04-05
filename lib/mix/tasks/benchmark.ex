defmodule Mix.Tasks.Benchmark do
  @moduledoc """
  Benchmarks all solved Advent of Code problems and generates a report.

  Usage: mix benchmark
  """
  use Mix.Task

  @impl Mix.Task
  def run(_args) do
    # Ensure the app is started so all modules are loaded
    Mix.Task.run("app.start")

    solutions = collect_solutions()
    
    if solutions == [] do
      Mix.shell().info("No solutions found to benchmark.")
    else
      Mix.shell().info("Benchmarking #{length(solutions)} solutions...")
      results = benchmark_all(solutions)
      generate_report(results)
    end
  end

  defp collect_solutions do
    # Scan lib directory for year/day folders
    "lib"
    |> File.ls!()
    |> Enum.filter(&Regex.match?(~r/^20\d{2}$/, &1))
    |> Enum.flat_map(fn year_dir ->
      "lib/#{year_dir}"
      |> File.ls!()
      |> Enum.filter(&Regex.match?(~r/^day_\d{2}\.ex$/, &1))
      |> Enum.map(fn day_file ->
        [_, day] = Regex.run(~r/day_(\d{2})\.ex/, day_file)
        {String.to_integer(year_dir), String.to_integer(day)}
      end)
    end)
    |> Enum.sort()
  end

  defp benchmark_all(solutions) do
    Enum.map(solutions, fn {year, day} ->
      Mix.shell().info("  Benchmarking #{year} Day #{day}...")

      case AdventOfCode.solve(year, day) do
        {:ok, {time_us, _result}} ->
          module = Module.concat([AdventOfCode, "Y#{year}", "Day#{String.pad_leading("#{day}", 2, "0")}"])
          meta = get_metadata(module)
          %{
            year: year,
            day: day,
            time_ms: time_us / 1000.0,
            title: meta.title,
            link: meta.link,
            difficulty: meta.difficulty
          }

        _ ->
          nil
      end
    end)
    |> Enum.filter(& &1)
  end

  defp get_metadata(module) do
    case Code.fetch_docs(module) do
      {:docs_v1, _, _, _, %{"en" => moduledoc}, _, _} ->
        parse_moduledoc(moduledoc)

      _ ->
        %{title: "Unknown", link: "#", difficulty: "m"}
    end
  end

  defp parse_moduledoc(doc) do
    title =
      case Regex.run(~r/--- Day \d+: (.*) ---/, doc) do
        [_, t] -> String.trim(t)
        _ -> "Unknown"
      end

    link =
      case Regex.run(~r/Problem Link: (.*)/, doc) do
        [_, l] -> String.trim(l)
        _ -> "#"
      end

    difficulty =
      case Regex.run(~r/Difficulty:\s*(.*)/, doc) do
        [_, d] -> String.downcase(String.trim(d))
        _ -> "m"
      end

    %{title: title, link: link, difficulty: difficulty}
  end

  defp generate_report(results) do
    lines = [
      "# ⚡ Benchmarks\n\n",
      "Performance results for all solved Advent of Code challenges.\n\n",
      "## System Information\n\n",
      "- **Run Date:** #{DateTime.utc_now() |> DateTime.to_string()}\n",
      "- **OS:** #{get_os()}\n",
      "- **CPU:** #{get_cpu()}\n\n",
      "## Performance Results\n\n"
    ]

    by_year = Enum.group_by(results, & &1.year)

    year_tables =
      by_year
      |> Map.keys()
      |> Enum.sort(:desc)
      |> Enum.map(fn year ->
        year_results = by_year[year] |> Enum.sort_by(& &1.day)
        year_total = Enum.reduce(year_results, 0, fn r, acc -> acc + r.time_ms end)

        table_rows =
          Enum.map(year_results, fn r ->
            "| #{r.day} | [#{r.title}](#{r.link}) | #{get_diff_icon(r.difficulty)} | #{Float.round(r.time_ms, 3)} |"
          end)

        header = """
        ### #{year}

        | Day | Title | Difficulty | Time (ms) |
        |:---:|-------|:----------:|----------:|
        """

        footer = "\n| **Total** | | | **#{Float.round(year_total, 3)}** |\n\n"

        [header, Enum.join(table_rows, "\n"), footer] |> Enum.join("")
      end)

    total_all_ms = Enum.reduce(results, 0, fn r, acc -> acc + r.time_ms end)
    footer = "**Total performance across all years: #{Float.round(total_all_ms / 1000.0, 3)}s**\n"

    content = Enum.concat([lines, year_tables, [footer]]) |> Enum.join("")
    File.write!("BENCHMARKS.md", content)
    Mix.shell().info("Benchmark report generated in BENCHMARKS.md")

    update_readme()
  end

  defp update_readme do
    if File.exists?("README.md") do
      content = File.read!("README.md")
      unless String.contains?(content, "BENCHMARKS.md") do
        new_content = String.replace(content, "· [Difficulty](wiki/difficulty.md)", "· [Difficulty](wiki/difficulty.md) · [Benchmarks](BENCHMARKS.md)")
        if new_content != content do
          File.write!("README.md", new_content)
          Mix.shell().info("Updated README.md with benchmark link.")
        end
      end
    end
  end

  defp get_diff_icon(diff) do
    case diff do
      "xs" -> "🟢"
      "s" -> "🟡"
      "m" -> "🟠"
      "l" -> "🔴"
      "xl" -> "💀"
      _ -> "⚪"
    end
  end

  defp get_os do
    case :os.type() do
      {:unix, :linux} -> "Linux"
      {:unix, :darwin} -> "macOS"
      {:win32, _} -> "Windows"
      _ -> "Unknown"
    end
  end

  defp get_cpu do
    try do
      {output, 0} = System.cmd("lscpu", [])
      output
      |> String.split("\n")
      |> Enum.find(fn line -> String.contains?(line, "Model name") end)
      |> case do
        nil -> "Unknown CPU"
        line -> line |> String.split(":") |> List.last() |> String.trim()
      end
    rescue
      _ -> "Unknown CPU"
    end
  end
end
