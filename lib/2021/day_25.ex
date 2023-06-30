defmodule AdventOfCode.Y2021.Day25 do
  @moduledoc """
  --- Day 25: Sea Cucumber ---
  Problem Link: https://adventofcode.com/2021/day/25
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2021, 25)

  def run(input \\ input()) do
    {grid, dims} = parse(input)

    {run_1(grid, dims, 1), "🎉"}
  end

  def parse(data \\ input()) do
    data
    |> String.split("\n")
    |> Enum.map(&String.graphemes/1)
    |> Transformers.grid2d()
    |> then(fn grid ->
      {max_row_idx, max_col_idx} = Enum.max(Map.keys(grid))
      {grid, {max_row_idx + 1, max_col_idx + 1}}
    end)
  end

  defp run_1(grid, dims, step) do
    grid
    |> move(">", dims)
    |> move("v", dims)
    |> then(fn
      ^grid -> step
      new_grid -> run_1(new_grid, dims, step + 1)
    end)
  end

  defp move(grid, facing, {rows, cols}) do
    for i <- 0..(rows - 1),
        j <- 0..(cols - 1),
        grid[{i, j}] == facing,
        next_position = target(facing, {i, j}, {rows, cols}),
        grid[next_position] == ".",
        reduce: grid do
      acc -> Map.merge(acc, %{{i, j} => ".", next_position => facing})
    end
  end

  defp target("v", {x, y}, {rows, _}), do: {rem(x + 1, rows), y}
  defp target(">", {x, y}, {_, cols}), do: {x, rem(y + 1, cols)}
end
