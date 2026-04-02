defmodule AdventOfCode.Y2025.Day02 do
  @moduledoc """
  --- Day 2: Gift Shop ---
  Problem Link: https://adventofcode.com/2025/day/2
  Difficulty: m
  Tags: simulation generation string-manipulation math
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2025, 2)

  def run(input \\ input()) do
    ranges = parse(input)
    merged = merge_ranges(ranges)

    max_val = merged |> Enum.map(fn {_, hi} -> hi end) |> Enum.max()
    max_len = max_val |> to_string() |> String.length()

    solution_1 = solve(merged, max_len, :exactly_twice)
    solution_2 = solve(merged, max_len, :at_least_twice)

    {solution_1, solution_2}
  end

  defp solve(ranges, max_len, rule) do
    candidates =
      for len <- 2..max_len,
          k <- find_multipliers(len, rule),
          p = div(len, k),
          min_x = Integer.pow(10, p - 1),
          max_x = Integer.pow(10, p) - 1,
          x <- min_x..max_x do
        s = to_string(x)
        String.duplicate(s, k) |> String.to_integer()
      end
      |> MapSet.new()

    candidates
    |> Enum.filter(fn id -> in_ranges?(id, ranges) end)
    |> Enum.sum()
  end

  defp find_multipliers(len, :exactly_twice) do
    if rem(len, 2) == 0, do: [2], else: []
  end

  defp find_multipliers(len, :at_least_twice) do
    for k <- 2..len, rem(len, k) == 0, do: k
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

  def parse(data \\ input()) do
    data
    |> String.split(~r/[, \n]/, trim: true)
    |> Enum.map(fn line ->
      [lo, hi] = String.split(line, "-")
      {String.to_integer(lo), String.to_integer(hi)}
    end)
  end
end
