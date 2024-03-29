defmodule AdventOfCode.Y2017.Day09 do
  @moduledoc """
  --- Day 9: Stream Processing ---
  Problem Link: https://adventofcode.com/2017/day/9
  Difficulty: s
  Tags: fsm
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2017, 9)

  def run(input \\ input()) do
    input = String.graphemes(input)

    {run_1(input), run_2(input)}
  end

  def run_1(input), do: process_groups(input)
  def run_2(input), do: process_garbages(input)

  def process_groups(data) do
    data
    |> groups(1, false, false, [])
    |> Enum.sum()
  end

  def process_garbages(data) do
    data
    |> garbages(0, false, false, [])
    |> Enum.sum()
  end

  defp groups([], _, _, _, groups), do: groups

  defp groups(["{" | data], level, false, false, groups) do
    groups(data, level + 1, false, false, [level | groups])
  end

  defp groups(["}" | data], level, false, false, groups) do
    groups(data, level - 1, false, false, groups)
  end

  defp groups(["," | data], level, false, false, groups) do
    groups(data, level, false, false, groups)
  end

  defp groups(["<" | data], level, false, false, groups) do
    groups(data, level, true, false, groups)
  end

  defp groups(["!" | data], level, true, false, groups) do
    groups(data, level, true, true, groups)
  end

  defp groups([_ | data], level, true, true, groups) do
    groups(data, level, true, false, groups)
  end

  defp groups([">" | data], level, true, false, groups) do
    groups(data, level, false, false, groups)
  end

  defp groups([_ | data], level, true, false, groups) do
    groups(data, level, true, false, groups)
  end

  defp garbages([], _, _, _, garbages), do: garbages

  defp garbages(["{" | data], count, false, false, garbages) do
    garbages(data, count, false, false, garbages)
  end

  defp garbages(["}" | data], count, false, false, garbages) do
    garbages(data, count, false, false, garbages)
  end

  defp garbages(["," | data], count, false, false, garbages) do
    garbages(data, count, false, false, garbages)
  end

  defp garbages(["<" | data], count, false, false, garbages) do
    garbages(data, count, true, false, garbages)
  end

  defp garbages(["!" | data], count, true, false, garbages) do
    garbages(data, count, true, true, garbages)
  end

  defp garbages([_ | data], count, true, true, garbages) do
    garbages(data, count, true, false, garbages)
  end

  defp garbages([">" | data], count, true, false, garbages) do
    garbages(data, 0, false, false, [count | garbages])
  end

  defp garbages([_ | data], count, true, false, garbages) do
    garbages(data, count + 1, true, false, garbages)
  end
end
