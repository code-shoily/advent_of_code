defmodule AdventOfCode.Y2025.Day01 do
  @moduledoc """
  --- Day 1: Secret Entrance ---
  Problem Link: https://adventofcode.com/2025/day/1
  Difficulty: e
  Tags: simulation safe-dial
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2025, 1)

  def run(input \\ input()) do
    instructions = parse(input)

    {solve_1(instructions), solve_2(instructions)}
  end

  defp solve_1(instructions) do
    {count, _final} =
      Enum.reduce(instructions, {0, 50}, fn {dir, steps}, {cnt, pos} ->
        delta = if dir == "L", do: -steps, else: steps
        new_pos = Integer.mod(pos + delta, 100)
        new_cnt = if new_pos == 0, do: cnt + 1, else: cnt
        {new_cnt, new_pos}
      end)

    count
  end

  defp solve_2(instructions) do
    {count, _final} =
      Enum.reduce(instructions, {0, 50}, fn {dir, steps}, {cnt, pos} ->
        hits = count_hits(pos, dir, steps)
        delta = if dir == "L", do: -steps, else: steps
        new_pos = Integer.mod(pos + delta, 100)
        {cnt + hits, new_pos}
      end)

    count
  end

  defp count_hits(pos, "R", steps) do
    Integer.floor_div(pos + steps, 100) - Integer.floor_div(pos, 100)
  end

  defp count_hits(pos, "L", steps) do
    Integer.floor_div(pos - 1, 100) - Integer.floor_div(pos - steps - 1, 100)
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      {side, steps} = String.split_at(line, 1)
      {side, String.to_integer(steps)}
    end)
  end
end
