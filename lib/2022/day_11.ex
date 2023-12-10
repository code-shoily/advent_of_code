defmodule AdventOfCode.Y2022.Day11 do
  @moduledoc """
  --- Day 11: Monkey in the Middle ---
  Problem Link: https://adventofcode.com/2022/day/11
  Difficulty: l
  Tags: erlang parse-heavy arithmetic
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 11)

  def run(input \\ input()) do
    :day_22_11.solve(input)
  end
end
