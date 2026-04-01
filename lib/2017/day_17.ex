defmodule AdventOfCode.Y2017.Day17 do
  @moduledoc """
  --- Day 17: Spinlock ---
  Problem Link: https://adventofcode.com/2017/day/17
  Difficulty: m
  Tags: circular-buffer simulation
  """
  def input, do: 394

  def run, do: {run_1(), run_2()}

  @target_1 2017
  defp run_1 do
    step = input()

    {buffer, _} =
      Enum.reduce(1..@target_1, {[0], 0}, fn i, {buf, pos} ->
        new_pos = rem(pos + step, i) + 1
        {List.insert_at(buf, new_pos, i), new_pos}
      end)

    idx = Enum.find_index(buffer, &(&1 == @target_1))
    Enum.at(buffer, rem(idx + 1, length(buffer)))
  end

  @target_2 50_000_000
  defp run_2 do
    step = input()

    {_, after_zero} =
      Enum.reduce(1..@target_2, {0, 0}, fn i, {pos, first} ->
        new_pos = rem(pos + step, i) + 1
        new_first = if new_pos == 1, do: i, else: first
        {new_pos, new_first}
      end)

    after_zero
  end
end
