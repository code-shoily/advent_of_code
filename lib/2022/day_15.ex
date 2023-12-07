defmodule AdventOfCode.Y2022.Day15 do
  @moduledoc """
  --- Day 15: Beacon Exclusion Zone ---
  Problem Link: https://adventofcode.com/2022/day/15
  Difficulty: xl
  Tags: erlang slow large-number geometry2d
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 15)

  def run(input \\ input()) do
    :day_22_15.solve(input)
  end
end
