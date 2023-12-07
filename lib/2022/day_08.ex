defmodule AdventOfCode.Y2022.Day08 do
  @moduledoc """
  --- Day 8: Treetop Tree House ---
  Problem Link: https://adventofcode.com/2022/day/8
  Difficulty: m
  Tags: erlang grid
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 8)

  def run(input \\ input()) do
    :day_22_8.solve(input)
  end
end
