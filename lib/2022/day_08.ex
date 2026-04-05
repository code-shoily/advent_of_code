defmodule AdventOfCode.Y2022.Day08 do
  @moduledoc """
  --- Day 8: Treetop Tree House ---
  Problem Link: https://adventofcode.com/2022/day/8
  Difficulty: m
  Tags: graph grid traversal
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Grid
  alias Yog.Traversal.Implicit

  def input, do: InputReader.read_from_file(2022, 8)

  def run(input \\ input()) do
    grid_data = input |> Transformers.lines() |> Enum.map(&Transformers.digits/1)
    grid = Grid.from_2d_list(grid_data, :directed, Grid.always())

    {part1(grid), part2(grid)}
  end

  defp part1(grid) do
    for r <- 0..(grid.rows - 1),
        c <- 0..(grid.cols - 1),
        visible?(grid, r, c),
        reduce: 0 do
      acc -> acc + 1
    end
  end

  defp part2(grid) do
    for r <- 0..(grid.rows - 1),
        c <- 0..(grid.cols - 1) do
      scenic_score(grid, r, c)
    end
    |> Enum.max()
  end

  defp visible?(grid, r, c) do
    if r == 0 or r == grid.rows - 1 or c == 0 or c == grid.cols - 1 do
      true
    else
      {:ok, h} = Grid.get_cell(grid, r, c)

      Enum.any?([{-1, 0}, {1, 0}, {0, -1}, {0, 1}], fn {dr, dc} ->
        directional_walk(grid, r + dr, c + dc, dr, dc)
        |> Enum.all?(fn other_h -> other_h < h end)
      end)
    end
  end

  defp scenic_score(grid, r, c) do
    {:ok, h} = Grid.get_cell(grid, r, c)

    Enum.reduce([{-1, 0}, {1, 0}, {0, -1}, {0, 1}], 1, fn {dr, dc}, acc ->
      acc * viewing_distance(grid, r + dr, c + dc, dr, dc, h)
    end)
  end

  defp directional_walk(grid, r, c, dr, dc) do
    Implicit.implicit_fold(
      from: {r, c},
      using: :breadth_first,
      successors_of: fn {curr_r, curr_c} ->
        nr = curr_r + dr
        nc = curr_c + dc
        if in_bounds?(grid, nr, nc), do: [{nr, nc}], else: []
      end,
      initial: [],
      with: fn acc, {curr_r, curr_c}, _meta ->
        {:ok, h} = Grid.get_cell(grid, curr_r, curr_c)
        {:continue, [h | acc]}
      end
    )
  end

  defp viewing_distance(grid, r, c, dr, dc, h) do
    if in_bounds?(grid, r, c) do
      Implicit.implicit_fold(
        from: {r, c},
        using: :breadth_first,
        successors_of: fn {curr_r, curr_c} ->
          {:ok, curr_h} = Grid.get_cell(grid, curr_r, curr_c)
          nr = curr_r + dr
          nc = curr_c + dc
          if curr_h < h and in_bounds?(grid, nr, nc), do: [{nr, nc}], else: []
        end,
        initial: 0,
        with: fn acc, _coord, _meta ->
          {:continue, acc + 1}
        end
      )
    else
      0
    end
  end

  defp in_bounds?(grid, r, c), do: r >= 0 and r < grid.rows and c >= 0 and c < grid.cols
end
