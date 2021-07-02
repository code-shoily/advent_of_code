defmodule AdventOfCode.Y2017.Day5 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2017/day/5
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 5

  def run_1, do: process() |> jump_1(0, 0)
  def run_2, do: process() |> jump_2(0, 0)
  def run, do: {run_1(), run_2()}

  def process(input \\ input!()) do
    input |> String.split("\n") |> Enum.map(&String.to_integer/1) |> as_map()
  end

  def as_map(instructions) do
    instructions |> Enum.with_index() |> Enum.map(&{elem(&1, 1), elem(&1, 0)}) |> Enum.into(%{})
  end

  def jump_1(map, x, steps), do: jump_1(map, map[x], x, steps)
  def jump_1(_, i, _, steps) when is_nil(i), do: steps
  def jump_1(map, i, x, steps), do: jump_1(Map.put(map, x, i + 1), x + i, steps + 1)

  def jump_2(map, x, steps), do: jump_2(map, map[x], x, steps)
  def jump_2(_, i, _, steps) when is_nil(i), do: steps
  def jump_2(map, i, x, steps) when i > 2, do: jump_2(Map.put(map, x, i - 1), x + i, steps + 1)
  def jump_2(map, i, x, steps), do: jump_2(Map.put(map, x, i + 1), x + i, steps + 1)
end
