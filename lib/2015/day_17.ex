defmodule AdventOfCode.Y2015.Day17 do
  @moduledoc """
  --- Day 17: No Such Thing as Too Much ---
  Problem Link: https://adventofcode.com/2015/day/17
  """
  alias AdventOfCode.Algorithms.Combinatorics
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 17)

  def run(input \\ input()) do
    combinations =
      input
      |> parse()
      |> find_matching_combinations()

    solution_1 = Task.async(fn -> Enum.count(combinations) end)
    solution_2 = Task.async(fn -> minimum_bottle_count(Enum.map(combinations, &length/1)) end)

    {
      Task.await(solution_1),
      Task.await(solution_2)
    }
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.to_integer/1)
  end

  defp find_matching_combinations(bottles) do
    1..length(bottles)
    |> Stream.flat_map(&Combinatorics.combinations(bottles, &1))
    |> Stream.filter(&(Enum.sum(&1) == 150))
  end

  defp minimum_bottle_count(containers) do
    Enum.count(containers, fn container -> container == Enum.min(containers) end)
  end
end
