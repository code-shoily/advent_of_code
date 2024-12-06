defmodule AdventOfCode.Y2024.Day05 do
  @moduledoc """
  --- Day 5: Print Queue ---
  Problem Link: https://adventofcode.com/2024/day/5
  Difficulty: xs
  Tags: set sort
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2024, 5)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    input
    |> Enum.filter(fn {a, b} -> a == b end)
    |> Enum.map(fn {a, _} -> a |> Enum.at(div(length(a), 2)) end)
    |> Enum.sum()
  end

  defp run_2(input) do
    input
    |> Enum.filter(fn {a, b} -> a != b end)
    |> Enum.map(fn {_, b} -> b |> Enum.at(div(length(b), 2)) end)
    |> Enum.sum()
  end

  def parse(data \\ input()) do
    [deps, updates] = Transformers.sections(data)
    given_sorted_pair({parse_deps(deps), parse_updates(updates)})
  end

  defp parse_deps(deps) do
    for line <- Transformers.lines(deps),
        into: MapSet.new(),
        do: String.split(line, "|") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
  end

  defp parse_updates(updates) do
    for line <- Transformers.lines(updates) do
      for update <- String.split(line, ","), do: String.to_integer(update)
    end
  end

  defp given_sorted_pair({deps, updates}) do
    for update <- updates do
      {update, Enum.sort(update, &({&1, &2} in deps))}
    end
  end
end
