defmodule AdventOfCode.Y2024.Day02 do
  @moduledoc """
  --- Day 2: Red-Nosed Reports ---
  Problem Link: https://adventofcode.com/2024/day/2
  Difficulty: s
  Tags: sequence validation
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2024, 2)

  def run(input \\ input()) do
    reports = parse(input)

    {run_1(reports), run_2(reports)}
  end

  defp run_1(reports) do
    Enum.count(reports, &safe?/1)
  end

  defp run_2(reports) do
    Enum.count(reports, fn report ->
      safe?(report) or
        0..(length(report) - 1)
        |> Enum.any?(fn i ->
          report |> List.delete_at(i) |> safe?()
        end)
    end)
  end

  defp safe?(report) do
    diffs =
      report
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.map(fn [a, b] -> b - a end)

    all_increasing = Enum.all?(diffs, fn d -> d >= 1 and d <= 3 end)
    all_decreasing = Enum.all?(diffs, fn d -> d <= -1 and d >= -3 end)

    all_increasing or all_decreasing
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      line
      |> String.split()
      |> Enum.map(&String.to_integer/1)
    end)
  end
end
