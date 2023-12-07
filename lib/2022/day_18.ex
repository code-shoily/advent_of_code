defmodule AdventOfCode.Y2022.Day18 do
  @moduledoc """
  --- Day 18: Boiling Boulders ---
  Problem Link: https://adventofcode.com/2022/day/18
  Difficulty: xl
  Tags: erlang geometry3d surface set
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 18)

  @spec run(binary()) :: {number(), number()}
  def run(input \\ input()) do
    :day_22_18.solve(input)
  end
end
