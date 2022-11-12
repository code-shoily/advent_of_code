defmodule AdventOfCode.Y2015.Day24 do
  @moduledoc """
  --- Day 24: It Hangs in the Balance ---
  Problem Link: https://adventofcode.com/2015/day/24
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 24

  alias AdventOfCode.Helpers.Transformers
  alias ExAlgo.DynamicProgramming.SubsetSum

  def run(input \\ input!()) do
    weights = parse(input)
    total = Enum.sum(weights)

    part_1 = Task.async(fn -> optimal_configuration(weights, div(total, 3)) end)
    part_2 = Task.async(fn -> optimal_configuration(weights, div(total, 4)) end)

    {
      Task.await(part_1, 10_000),
      Task.await(part_2, 10_000)
    }
  end

  def optimal_configuration(weights, target) do
    configurations = SubsetSum.find_subsets(weights, target)
    smallest_length = smallest_length(configurations)

    configurations
    |> Enum.filter(&(length(&1) == smallest_length))
    |> Enum.min_by(&Enum.product/1)
    |> Enum.product()
  end

  def parse(data \\ input!()) do
    Transformers.int_lines(data)
  end

  def smallest_length(list_of_list) do
    list_of_list |> Enum.min_by(&length/1) |> length()
  end
end
