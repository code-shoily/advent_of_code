defmodule AdventOfCode.Y2022.Day09 do
  @moduledoc """
  --- Day 9: Rope Bridge ---
  Problem Link: https://adventofcode.com/2022/day/9
  Difficulty: m
  Tags: grid walk
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 9)

  def run(input \\ input()) do
    instructions = parse(input)
    {part1(instructions), part2(instructions)}
  end

  defp part1(instructions), do: solve(instructions, 2)
  defp part2(instructions), do: solve(instructions, 10)

  defp solve(instructions, count) do
    start_rope = List.duplicate({0, 0}, count)

    {_rope, visited} =
      Enum.reduce(instructions, {start_rope, MapSet.new([{0, 0}])}, fn {dir, steps}, acc ->
        Enum.reduce(1..steps, acc, fn _, {rope, visited} ->
          [head | tails] = rope
          new_head = move_head(head, dir)

          # Each knot follows the one before it
          new_rope =
            Enum.scan(tails, new_head, fn tail, prev ->
              follow(prev, tail)
            end)

          full_rope = [new_head | new_rope]
          {full_rope, MapSet.put(visited, List.last(full_rope))}
        end)
      end)

    MapSet.size(visited)
  end

  defp parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [dir, steps] = String.split(line, " ")
      {dir, String.to_integer(steps)}
    end)
  end

  defp move_head({r, c}, "U"), do: {r - 1, c}
  defp move_head({r, c}, "D"), do: {r + 1, c}
  defp move_head({r, c}, "L"), do: {r, c - 1}
  defp move_head({r, c}, "R"), do: {r, c + 1}

  defp follow({hr, hc}, {tr, tc}) do
    dr = hr - tr
    dc = hc - tc

    if abs(dr) <= 1 and abs(dc) <= 1 do
      {tr, tc}
    else
      {tr + sign(dr), tc + sign(dc)}
    end
  end

  defp sign(0), do: 0
  defp sign(n) when n > 0, do: 1
  defp sign(n) when n < 0, do: -1
end
