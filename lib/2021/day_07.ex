defmodule AdventOfCode.Y2021.Day07 do
  @moduledoc """
  --- Day 7: The Treachery of Whales ---
  Problem Link: https://adventofcode.com/2021/day/7
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 7

  def run_1, do: input!() |> parse() |> alignments(&fixed_cost/2) |> Enum.min()
  def run_2, do: input!() |> parse() |> alignments(&cost/2) |> Enum.min()

  def parse(data) do
    data
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  defp alignments(positions, cost_fn) do
    {min, max} = Enum.min_max(positions)

    for cur <- min..max do
      for pos <- positions, reduce: 0 do
        cost -> cost + cost_fn.(cur, pos)
      end
    end
  end

  defp fixed_cost(current, position), do: abs(position - current)

  defp cost(current, position) do
    steps = abs(position - current)
    (steps == 0 && 0) || div(steps * (steps + 1), 2)
  end
end
