defmodule AdventOfCode.Y2024.Day04 do
  @moduledoc """
  --- Day 4: Ceres Search ---
  Problem Link: https://adventofcode.com/2024/day/4
  Difficulty: s
  Tags: grid
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  alias Yog.Builder.Grid

  def input, do: InputReader.read_from_file(2024, 4)

  def run(input \\ input()) do
    grid = parse_to_map(input)

    {run_1(grid), run_2(grid)}
  end

  defp run_1(grid) do
    for {{x, y}, "X"} <- grid,
        {dx, dy} <- Grid.queen(),
        xmas?(grid, x, y, dx, dy),
        reduce: 0 do
      acc -> acc + 1
    end
  end

  defp run_2(grid) do
    for {{x, y}, "A"} <- grid,
        x_mas?(grid, x, y),
        reduce: 0 do
      acc -> acc + 1
    end
  end

  defp xmas?(grid, x, y, dx, dy) do
    grid[{x + dx, y + dy}] == "M" and
      grid[{x + 2 * dx, y + 2 * dy}] == "A" and
      grid[{x + 3 * dx, y + 3 * dy}] == "S"
  end

  defp x_mas?(grid, x, y) do
    d1 = {grid[{x - 1, y - 1}], grid[{x + 1, y + 1}]}
    d2 = {grid[{x + 1, y - 1}], grid[{x - 1, y + 1}]}

    valid_diag?(d1) and valid_diag?(d2)
  end

  defp valid_diag?({"M", "S"}), do: true
  defp valid_diag?({"S", "M"}), do: true
  defp valid_diag?(_), do: false

  defp parse_to_map(data) do
    lines = Transformers.lines(data)

    for {line, y} <- Enum.with_index(lines),
        {char, x} <- Enum.with_index(String.graphemes(line)),
        into: %{} do
      {{x, y}, char}
    end
  end

  def parse(data \\ input()) do
    data |> Transformers.lines()
  end
end
