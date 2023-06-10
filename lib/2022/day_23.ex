defmodule AdventOfCode.Y2022.Day23 do
  @moduledoc """
  --- Day 23: Unstable Diffusion ---
  Problem Link: https://adventofcode.com/2022/day/23
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2022, 23)

  def run(input \\ input()) do
    :day_22_23.solve(input)
  end
end
