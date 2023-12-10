defmodule AdventOfCode.Y2017.Day17 do
  @moduledoc """
  --- Day 17: Spinlock ---
  Problem Link: https://adventofcode.com/2017/day/17
  Difficulty: m
  Tags: vector random-access not-fast-enough
  """
  alias Aja.Vector

  def input, do: 394

  def run, do: {run_1(), run_2()}

  @target 2017
  defp run_1 do
    Vector.new([0])
    |> insert_nth(0, 0)
    |> then(fn buffer ->
      Vector.at(buffer, Aja.Enum.find_index(buffer, &(&1 == 2017)) + 1)
    end)
  end

  defp insert_nth(buffer, _, @target), do: buffer

  defp insert_nth(buffer, position, idx) do
    idx = idx + 1
    position = rem(position + input(), idx) + 1
    {left, right} = {Vector.take(buffer, position), Vector.drop(buffer, position)}
    buffer = left |> Vector.concat(Vector.new([idx])) |> Vector.concat(right)

    insert_nth(buffer, position, idx)
  end

  @target 50_000_000
  defp run_2, do: find_idx_1_after(0, 0, 0)

  defp find_idx_1_after(_, @target, first), do: first

  defp find_idx_1_after(position, idx, first) do
    idx = idx + 1
    position = rem(position + input(), idx) + 1
    first = (position == 1 && idx) || first
    find_idx_1_after(position, idx, first)
  end
end
