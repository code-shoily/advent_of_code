defmodule AdventOfCode.Y2023.Day11 do
  @moduledoc """
  --- Day 11: Cosmic Expansion ---
  Problem Link: https://adventofcode.com/2023/day/11
  Difficulty: s
  Tags: grid measurement
  """
  alias AdventOfCode.Algorithms.Grid
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 11)

  def run(input \\ input()) do
    input = parse(input)
    {all_pairs_distance(input, 2), all_pairs_distance(input, 1_000_000)}
  end

  def parse(data \\ input()) do
    grid =
      data
      |> Transformers.lines()
      |> Enum.map(&String.graphemes/1)
      |> Grid.grid2d()

    {{row_max, col_max}, _} = Enum.max(grid)
    galaxies = for {p, "#"} <- grid, do: p
    occupied_rows = for {row, _} <- galaxies, into: %MapSet{}, do: row
    occupied_cols = for {_, col} <- galaxies, into: %MapSet{}, do: col
    empty_rows = 0..row_max |> MapSet.new() |> MapSet.difference(occupied_rows)
    empty_cols = 0..col_max |> MapSet.new() |> MapSet.difference(occupied_cols)

    {galaxies, empty_rows, empty_cols}
  end

  defp all_pairs_distance({galaxies, empty_rows, empty_cols}, rate) do
    galaxies
    |> expand(empty_rows, empty_cols, rate)
    |> then(fn [current | remaining] -> all_pairs_distance(0, current, remaining) end)
  end

  defp all_pairs_distance(distance, _, []), do: distance

  defp all_pairs_distance(distance, {x1, y1}, [next | remaining] = galaxies) do
    for {x2, y2} <- galaxies, reduce: distance do
      acc -> acc + abs(x2 - x1) + abs(y2 - y1)
    end
    |> all_pairs_distance(next, remaining)
  end

  defp expand(galaxies, rows, cols, rate) do
    for {x, y} <- galaxies,
        do:
          {x + (rate - 1) * Enum.count(rows, &(&1 < x)),
           y + (rate - 1) * Enum.count(cols, &(&1 < y))}
  end
end
