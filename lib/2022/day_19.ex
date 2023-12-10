defmodule AdventOfCode.Y2022.Day19 do
  @moduledoc """
  --- Day 19: Not Enough Minerals ---
  Problem Link: https://adventofcode.com/2022/day/19
  Difficulty: xl
  Tags: erlang slow needs-improvement grid walk optimization
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 19)

  def run(input \\ input()) do
    :day_22_19.solve(input)
  end
end
