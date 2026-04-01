defmodule AdventOfCode.Y2017.Day05 do
  @moduledoc """
  --- Day 5: A Maze of Twisty Trampolines, All Alike ---
  Problem Link: https://adventofcode.com/2017/day/5
  Difficulty: s
  Tags: atomics random-access
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 5)

  def run(input \\ input()) do
    parsed = input |> Transformers.lines() |> Enum.map(&String.to_integer/1)

    t1 =
      Task.async(fn ->
        # Clean start for the process dictionary
        Enum.with_index(parsed) |> Enum.each(fn {v, i} -> :erlang.put(i, v) end)
        jump_1(0, 0)
      end)

    t2 =
      Task.async(fn ->
        Enum.with_index(parsed) |> Enum.each(fn {v, i} -> :erlang.put(i, v) end)
        jump_2(0, 0)
      end)

    {Task.await(t1, :infinity), Task.await(t2, :infinity)}
  end

  defp jump_1(pc, steps) do
    case :erlang.get(pc) do
      :undefined ->
        steps

      offset ->
        :erlang.put(pc, offset + 1)
        jump_1(pc + offset, steps + 1)
    end
  end

  defp jump_2(pc, steps) do
    case :erlang.get(pc) do
      :undefined ->
        steps

      offset ->
        new_val = if offset >= 3, do: offset - 1, else: offset + 1
        :erlang.put(pc, new_val)
        jump_2(pc + offset, steps + 1)
    end
  end
end
