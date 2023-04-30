defmodule AdventOfCode.Y2020.Day06 do
  @moduledoc """
  --- Day 6: Custom Customs ---
  Problem Link: https://adventofcode.com/2020/day/6
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2020, 6)

  def run(input \\ input()) do
    input = parse(input)
    {reducer(input, &answers/1), reducer(input, &unanimous_answers/1)}
  end

  def reducer(input, mapper), do: Enum.reduce(input, 0, &(mapper.(&1) + &2))

  def parse(input) do
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
