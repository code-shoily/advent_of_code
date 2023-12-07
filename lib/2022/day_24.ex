defmodule AdventOfCode.Y2022.Day24 do
  @moduledoc """
  --- Day 24: Blizzard Basin ---
  Problem Link: https://adventofcode.com/2022/day/24
  Difficulty: m
  Tags: erlang slow optimization grid search
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 24)

  def run(input \\ input()) do
    :day_22_24.solve(input)
  end
end
