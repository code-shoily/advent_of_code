defmodule AdventOfCode.Y2023.Day17 do
  @moduledoc """
  --- Day 17: Clumsy Crucible ---
  Problem Link: https://adventofcode.com/2023/day/17
  Difficulty:
  Tags:
  """
  alias AdventOfCode.Algorithms.Grid
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 17)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(_input) do
    {:todo, 1}
  end

  defp run_2(_input) do
    {:todo, 2}
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.graphemes/1)
    |> Grid.grid2d(&String.to_integer/1)
    |> then(fn map ->
      {max_row, _} = Enum.max_by(Map.keys(map), &elem(&1, 0))
      {_, max_col} = Enum.max_by(Map.keys(map), &elem(&1, 1))
      {{0..max_row, 0..max_col}, map}
    end)
  end
end
