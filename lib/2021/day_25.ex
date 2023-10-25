defmodule AdventOfCode.Y2021.Day25 do
  @moduledoc """
  --- Day 25: Sea Cucumber ---
  Problem Link: https://adventofcode.com/2021/day/25
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Aja.Vector

  def input, do: InputReader.read_from_file(2021, 25)
  def run(input \\ input()), do: {run_1(parse(input), 1), "🎉"}

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(&(&1 |> String.graphemes() |> Vector.new()))
    |> then(fn [x | _] = xs -> {Vector.new(xs), {length(xs), Vector.size(x)}} end)
  end

  defp run_1({grid, dims}, step) do
    new_grid = grid |> move(">", dims) |> move("v", dims)
    (new_grid == grid && step) || run_1({new_grid, dims}, step + 1)
  end

  defp move(grid, facing, {rows, cols}) do
    for i <- 0..(rows - 1),
        j <- 0..(cols - 1),
        grid[i][j] == facing,
        {x, y} =
          (case facing do
             "v" -> {rem(i + 1, rows), j}
             ">" -> {i, rem(j + 1, cols)}
           end),
        grid[x][y] == ".",
        reduce: grid do
      acc -> acc |> put_in([i, j], ".") |> put_in([x, y], facing)
    end
  end
end
