defmodule AdventOfCode.Y2022.Day10 do
  @moduledoc """
  --- Day 10: Cathode-Ray Tube ---
  Problem Link: https://adventofcode.com/2022/day/10
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 10)

  def run(input \\ input()) do
    :day_22_10.solve(input)
  end
end