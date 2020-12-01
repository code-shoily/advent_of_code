defmodule AdventOfCode.Y2020.Day1 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/1
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 1

  def process(input \\ input!()) do
    input
    |> String.split("\n", trim: true)
  end

  def run_1 do
    {:not_implemented, 1}
  end

  def run_2 do
    {:not_implemented, 2}
  end

  def run, do: {run_1(), run_2()}
end
