defmodule Mix.Tasks.Benchmark do
  @moduledoc """
  Benchmarks Advent of Code problems and updates JSON data.

  Usage:
    mix benchmark              - Benchmarks everything
    mix benchmark 2025         - Benchmarks all days in 2025
    mix benchmark 2025 1       - Benchmarks only 2025 day 1
  """
  use Mix.Task

  @json_file "priv/data/benchmark.json"

  @impl Mix.Task
  def run(args) do
    # Ensure the app is started
    Mix.Task.run("app.start")
    File.mkdir_p!(Path.dirname(@json_file))

    parsed_args = parse_args(args)
    solutions = collect_solutions(parsed_args)

    if solutions == [] do
      Mix.shell().info("No solutions found matching criteria.")
    else
      Mix.shell().info("Benchmarking #{length(solutions)} solutions...")
      new_results = benchmark_all(solutions)
      update_json(new_results)
    end
  end

  defp parse_args([]), do: :all
  defp parse_args([year]), do: {:year, String.to_integer(year)}
  defp parse_args([year, day]), do: {:day, String.to_integer(year), String.to_integer(day)}

  defp collect_solutions(:all) do
    list_all_solutions()
  end

  defp collect_solutions({:year, year}) do
    list_solutions_for_year(year)
  end

  defp collect_solutions({:day, year, day}) do
    [{year, day}]
  end

  defp list_all_solutions do
    "lib"
    |> File.ls!()
    |> Enum.filter(&Regex.match?(~r/^20\d{2}$/, &1))
    |> Enum.flat_map(fn year_dir ->
      list_solutions_for_year(String.to_integer(year_dir))
    end)
    |> Enum.sort()
  end

  defp list_solutions_for_year(year) do
    dir = "lib/#{year}"
    if File.dir?(dir) do
      dir
      |> File.ls!()
      |> Enum.filter(&Regex.match?(~r/^day_\d{2}\.ex$/, &1))
      |> Enum.map(fn day_file ->
        [_, day] = Regex.run(~r/day_(\d{2})\.ex/, day_file)
        {year, String.to_integer(day)}
      end)
    else
      []
    end
  end

  defp benchmark_all(solutions) do
    Enum.map(solutions, fn {year, day} ->
      Mix.shell().info("  Benchmarking #{year} Day #{day}...")

      case AdventOfCode.solve(year, day) do
        {:ok, {time_us, _result}} ->
          module = Module.concat([AdventOfCode, "Y#{year}", "Day#{String.pad_leading("#{day}", 2, "0")}"])
          meta = get_metadata(module)
          %{
            "year" => year,
            "day" => day,
            "time_ms" => Float.round(time_us / 1000.0, 3),
            "title" => meta.title,
            "link" => meta.link,
            "difficulty" => meta.difficulty
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
    title = case Regex.run(~r/--- Day \d+: (.*) ---/, doc) do
      [_, t] -> String.trim(t)
      _ -> "Unknown"
    end
    link = case Regex.run(~r/Problem Link: (.*)/, doc) do
      [_, l] -> String.trim(l)
      _ -> "#"
    end
    difficulty = case Regex.run(~r/Difficulty:\s*(.*)/, doc) do
      [_, d] -> String.downcase(String.trim(d))
      _ -> "m"
    end
    %{title: title, link: link, difficulty: difficulty}
  end

  defp update_json(new_results) do
    existing_data = if File.exists?(@json_file) do
      @json_file |> File.read!() |> Jason.decode!()
    else
      %{}
    end

    updated_data = Enum.reduce(new_results, existing_data, fn res, acc ->
      year_str = to_string(res["year"])
      day_str = to_string(res["day"])
      
      year_map = Map.get(acc, year_str, %{})
      updated_year_map = Map.put(year_map, day_str, res)
      Map.put(acc, year_str, updated_year_map)
    end)

    File.write!(@json_file, Jason.encode!(updated_data, pretty: true))
    Mix.shell().info("Benchmark data updated in #{@json_file}")
  end
end
