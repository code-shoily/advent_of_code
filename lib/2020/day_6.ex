defmodule AdventOfCode.Y2020.Day6 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/6
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 6

  def run_1, do: input!() |> process() |> Enum.map(&answers/1) |> Enum.sum()
  def run_2, do: input!() |> process() |> Enum.map(&unanimous_answers/1) |> Enum.sum()
  def run, do: {run_1(), run_2()}

  def process(input) do
    input
    |> String.split("\n")
    |> Enum.chunk_by(&(&1 == ""))
    |> Enum.reject(&(&1 == [""]))
  end

  defp answers(group) do
    group
    |> Enum.flat_map(&String.graphemes/1)
    |> Enum.uniq()
    |> Enum.count()
  end

  defp unanimous_answers(group) do
    group
    |> Enum.map(&MapSet.new(String.graphemes(&1)))
    |> Enum.reduce(&MapSet.intersection/2)
    |> Enum.count()
  end
end
