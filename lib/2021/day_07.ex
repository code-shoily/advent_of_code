defmodule AdventOfCode.Y2021.Day07 do
  @moduledoc """
  --- Day 7: The Treachery of Whales ---
  Problem Link: https://adventofcode.com/2021/day/7
  Difficulty: xs
  Tags: calculation
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2021, 7)

  def run(input \\ input()) do
    input = Transformers.int_words(input, ",")
    {run_1(input), run_2(input)}
  end

  def run_1(input), do: input |> alignments(&fixed_cost/2) |> Enum.min()
  def run_2(input), do: input |> alignments(&cost/2) |> Enum.min()

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
