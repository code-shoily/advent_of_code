defmodule AdventOfCode.Y2020.Day03 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/3
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 3

  alias AdventOfCode.Helpers.Transformers

  @default_slope {3, 1}
  @slopes [{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}]

  def run_1, do: input!() |> process() |> traverse()

  def run_2 do
    for result <- Enum.map(@slopes, &traverse(process(input!()), &1)),
        reduce: 1 do
      acc -> acc * result
    end
  end

  def run, do: {run_1(), run_2()}

  def process(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
    |> (&{Transformers.grid2d(&1), length(hd(&1)), length(&1)}).()
  end

  defp traverse(xy, slope \\ @default_slope), do: traverse(xy, 0, 0, 0, slope)

  defp traverse(
         {_, _, col_size},
         row,
         _,
         result,
         _
       )
       when row > col_size,
       do: result

  defp traverse(
         {xy, row_size, _} = world,
         row,
         col,
         result,
         {right, down} = slope
       ) do
    traverse(
      world,
      row + down,
      rem(col + right, row_size),
      result + ((xy[row][col] == "#" && 1) || 0),
      slope
    )
  end
end
