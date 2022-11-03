defmodule AdventOfCode.Y2015.Day17 do
  @moduledoc """
  --- Day 17: No Such Thing as Too Much ---
  Problem Link: https://adventofcode.com/2015/day/17
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 17

  alias AdventOfCode.Helpers.Combinatorics

  def run(input \\ input!()) do
    combinations =
      input
      |> parse()
      |> find_matching_combinations()

    {
      Enum.count(combinations),
      minimum_bottle_count(Enum.map(combinations, &length/1))
    }
  end

  def parse(data) do
    data
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  defp find_matching_combinations(bottles) do
    1..length(bottles)
    |> Stream.flat_map(&Combinatorics.combinations(bottles, &1))
    |> Stream.filter(&(Enum.sum(&1) == 150))
  end

  defp minimum_bottle_count(containers) do
    smallest = Enum.min(containers)

    containers
    |> Enum.filter(&(&1 == smallest))
    |> Enum.count()
  end
end
