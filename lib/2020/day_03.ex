defmodule AdventOfCode.Y2020.Day03 do
  @moduledoc """
  --- Day 3: Toboggan Trajectory ---
  Problem Link: https://adventofcode.com/2020/day/3
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @default_slope {3, 1}
  @slopes [{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}]

  def input, do: InputReader.read_from_file(2020, 3)

  def run(input \\ input()) do
    input = parse(input)
    {run_1(input), run_2(input)}
  end

  def run_1(input), do: traverse(input)

  def run_2(input) do
    for result <- Enum.map(@slopes, &traverse(input, &1)), reduce: 1 do
      acc -> acc * result
    end
  end

  def parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(&String.graphemes/1)
    |> then(&{Transformers.grid2d(&1), length(hd(&1)), length(&1)})
  end

  defp traverse(xy, slope \\ @default_slope), do: traverse(xy, 0, 0, 0, slope)
  defp traverse({_, _, cols}, r, _, res, _) when r > cols, do: res

  defp traverse({yx, rows, _} = world, y, x, res, {right, down} = slope) do
    traverse(world, y + down, rem(x + right, rows), res + ((yx[{y, x}] == "#" && 1) || 0), slope)
  end
end
