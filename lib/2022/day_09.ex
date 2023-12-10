defmodule AdventOfCode.Y2022.Day09 do
  @moduledoc """
  --- Day 9: Rope Bridge ---
  Problem Link: https://adventofcode.com/2022/day/9
  Difficulty: m
  Tags: erlang grid walk
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 9)

  def run(input \\ input()) do
    :day_22_9.solve(input)
  end
end
