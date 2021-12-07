defmodule AdventOfCode.Y2021.Day07 do
  @moduledoc """
  --- Day 7: The Treachery of Whales ---
  Problem Link: https://adventofcode.com/2021/day/7
  Tweet sized version: https://twitter.com/mafinar/status/1468100177164746753
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 7

  def run_1, do: input!() |> parse() |> alignments() |> Enum.min()
  def run_2, do: input!() |> parse() |> alignments(&cost/1) |> Enum.min()

  def parse(data) do
    data
    |> String.split(",")
    |> Enum.map(&String.to_integer/1)
  end

  defp alignments(positions, cost_fn \\ &Function.identity/1) do
    {min, max} = Enum.min_max(positions)

    for cur <- min..max do
      Enum.sum(for pos <- positions, do: cost_fn.(abs(cur - pos)))
    end
  end

  defp cost(steps), do: (steps == 0 && 0) || div(steps * (steps + 1), 2)
end
