defmodule AdventOfCode.Y2025.Day05 do
  @moduledoc """
  --- Day 5: Cafeteria ---
  Problem Link: https://adventofcode.com/2025/day/5
  Difficulty: s
  Tags: simulation ranges interval-merging
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2025, 5)

  def run(input \\ input()) do
    [ranges_raw, ids_raw] = String.split(input, ~r/\n\s*\n/, parts: 2)
    ranges = parse_ranges(ranges_raw)
    ids = parse_ids(ids_raw)

    {run_1(ranges, ids), run_2(ranges)}
  end

  defp run_1(ranges, ids) do
    merged = merge_ranges(ranges)

    ids
    |> Enum.count(fn id -> in_ranges?(id, merged) end)
  end

  defp run_2(ranges) do
    ranges
    |> merge_ranges()
    |> Enum.map(fn {lo, hi} -> hi - lo + 1 end)
    |> Enum.sum()
  end

  defp in_ranges?(id, ranges) do
    Enum.any?(ranges, fn {lo, hi} -> id >= lo and id <= hi end)
  end

  defp merge_ranges(ranges) do
    ranges
    |> Enum.sort()
    |> Enum.reduce([], fn
      {lo, hi}, [] ->
        [{lo, hi}]

      {lo, hi}, [{prev_lo, prev_hi} | rest] ->
        if lo <= prev_hi + 1 do
          [{prev_lo, max(hi, prev_hi)} | rest]
        else
          [{lo, hi}, {prev_lo, prev_hi} | rest]
        end
    end)
    |> Enum.reverse()
  end

  defp parse_ranges(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [lo, hi] = String.split(line, "-")
      {String.to_integer(lo), String.to_integer(hi)}
    end)
  end

  defp parse_ids(data) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.to_integer/1)
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
  end
end
