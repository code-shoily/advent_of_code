defmodule AdventOfCode.Y2023.Day03 do
  @moduledoc """
  --- Day 3: Gear Ratios ---
  Problem Link: https://adventofcode.com/2023/day/3
  Difficulty: m
  Tags: grid-walk
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 3)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    input
    |> Enum.map(fn {v, _, _} -> String.to_integer(v) end)
    |> Enum.sum()
  end

  defp run_2(input) do
    input
    |> Enum.map(fn {v, _, gear} -> {String.to_integer(v), Enum.uniq(gear)} end)
    |> Enum.flat_map(fn {num, gears} ->
      gears
      |> Enum.map(fn gear ->
        {gear, num}
      end)
    end)
    |> Enum.group_by(fn {a, _} -> a end, fn {_, b} -> b end)
    |> Map.filter(fn {_, v} -> length(v) == 2 end)
    |> Map.values()
    |> Enum.map(fn [a, b] -> a * b end)
    |> Enum.sum()
  end

  def parse(data \\ input()) do
    grid = data |> Transformers.lines() |> Enum.map(&String.graphemes/1) |> Transformers.grid2d()

    0..(grid |> Map.keys() |> Enum.max() |> elem(0))
    |> Enum.flat_map(&collect_all(grid, &1))
    |> Enum.filter(fn {_, n, _} -> n == true end)
  end

  defp dirs(x, y) do
    [
      {x + 1, y},
      {x - 1, y},
      {x, y + 1},
      {x, y - 1},
      {x + 1, y + 1},
      {x - 1, y - 1},
      {x + 1, y - 1},
      {x - 1, y + 1}
    ]
  end

  @reject MapSet.new(~w/. 1 2 3 4 5 6 7 8 9 0/)
  defp is_part(grid, {x, y}) do
    dirs(x, y)
    |> Enum.map(&grid[&1])
    |> Enum.reject(&(is_nil(&1) || MapSet.member?(@reject, &1)))
    |> Enum.empty?()
    |> Kernel.not()
  end

  defp get_gears(grid, {x, y}), do: Enum.filter(dirs(x, y), &(grid[&1] == "*"))

  defp collect_one(grid, pos), do: collect_one(grid[pos], grid, pos, "", false, [])

  defp collect_one(cur, grid, {x, y}, digits, part?, gears) when cur in ~w/1 2 3 4 5 6 7 8 9 0/ do
    collect_one(
      grid[{x, y + 1}],
      grid,
      {x, y + 1},
      digits <> cur,
      part? || is_part(grid, {x, y}),
      get_gears(grid, {x, y}) ++ gears
    )
  end

  defp collect_one(nil, _, {_, _}, digits, part?, gears), do: {:halt, digits, part?, gears}
  defp collect_one(_, _, {_, y}, digits, part?, gears), do: {{:cont, y}, digits, part?, gears}

  defp collect_all(grid, row), do: collect_all(grid, row, 0, [])

  defp collect_all(grid, row, col, numbers) do
    case collect_one(grid, {row, col}) do
      {:halt, "", _, _} ->
        numbers

      {_, "", _, _} ->
        collect_all(grid, row, col + 1, numbers)

      {:halt, number, part?, gears} ->
        [{number, part?, gears} | numbers]

      {{:cont, next}, number, part?, gears} ->
        collect_all(grid, row, next, [{number, part?, gears} | numbers])
    end
  end
end
