defmodule AdventOfCode.Y2017.Day05 do
  @moduledoc """
  --- Day 5: A Maze of Twisty Trampolines, All Alike ---
  Problem Link: https://adventofcode.com/2017/day/5
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 5

  def run_1, do: input!() |> parse() |> then(fn _ -> jump_1(0, 0) end)
  def run_2, do: input!() |> parse() |> then(fn _ -> jump_2(0, 0) end)
  def run, do: {run_1(), run_2()}

  def parse(input) do
    input
    |> String.split("\n")
    |> Enum.with_index()
    |> Enum.each(fn {v, k} -> Process.put(k, String.to_integer(v)) end)
  end

  def jump_1(x, steps), do: jump_1(Process.get(x), x, steps)
  def jump_1(i, _, steps) when is_nil(i), do: steps
  def jump_1(i, x, steps), do: Process.put(x, i + 1) |> then(fn _ -> jump_1(x + i, steps + 1) end)

  def jump_2(x, steps), do: jump_2(Process.get(x), x, steps)
  def jump_2(i, _, steps) when is_nil(i), do: steps

  def jump_2(i, x, steps),
    do: Process.put(x, (i > 2 && i - 1) || i + 1) |> then(fn _ -> jump_2(x + i, steps + 1) end)
end
