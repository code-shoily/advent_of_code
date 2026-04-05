defmodule AdventOfCode.Y2016.Day17 do
  @moduledoc """
  --- Day 17: Two Steps Forward ---
  Problem Link: https://adventofcode.com/2016/day/17
  Difficulty: m
  Tags: bfs graph md5 state-space-search
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2016, 17)

  @target {3, 3}
  @directions [
    {"U", 0, -1, 0},
    {"D", 0, 1, 1},
    {"L", -1, 0, 2},
    {"R", 1, 0, 3}
  ]
  @open_chars ["b", "c", "d", "e", "f"]

  def run(input \\ input()) do
    passcode = input |> String.trim()

    solution_1 = solve_part1(passcode)
    solution_2 = solve_part2(passcode)

    {solution_1, solution_2}
  end

  defp solve_part1(passcode) do
    Yog.Traversal.implicit_fold(
      from: {0, 0, ""},
      using: :breadth_first,
      initial: nil,
      successors_of: fn state -> successors(state, passcode) end,
      with: fn _acc, {x, y, path}, _meta ->
        if {x, y} == @target do
          {:halt, path}
        else
          {:continue, nil}
        end
      end
    )
  end

  defp solve_part2(passcode) do
    Yog.Traversal.implicit_fold(
      from: {0, 0, ""},
      using: :breadth_first,
      initial: 0,
      successors_of: fn state -> successors(state, passcode) end,
      with: fn max_len, {x, y, path}, _meta ->
        if {x, y} == @target do
          {:continue, max(max_len, String.length(path))}
        else
          {:continue, max_len}
        end
      end
    )
  end

  defp successors({x, y, path}, passcode) do
    if {x, y} == @target do
      []
    else
      hash_prefix =
        :crypto.hash(:md5, passcode <> path)
        |> Base.encode16(case: :lower)
        |> String.slice(0, 4)
        |> String.graphemes()

      for {dir, dx, dy, idx} <- @directions,
          char = Enum.at(hash_prefix, idx),
          char in @open_chars,
          nx = x + dx,
          ny = y + dy,
          nx in 0..3,
          ny in 0..3,
          do: {nx, ny, path <> dir}
    end
  end
end
