defmodule AdventOfCode.Y2022.Day04 do
  @moduledoc """
  --- Day 4: Camp Cleanup ---
  Problem Link: https://adventofcode.com/2022/day/4
  Difficulty: xs
  Tags: range set
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 4)

  def run(input \\ input()) do
    with input <- parse(input), do: {run_1(input), run_2(input)}
  end

  defp run_1(data),
    do: Enum.reduce(data, 0, fn [a, b], acc -> acc + ((subset?(a, b) && 1) || 0) end)

  defp run_2(data),
    do: Enum.reduce(data, 0, fn [a, b], acc -> acc + ((MapSet.disjoint?(a, b) && 0) || 1) end)

  defp subset?(a, b), do: MapSet.subset?(a, b) || MapSet.subset?(b, a)

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      line
      |> String.split(",")
      |> Enum.map(fn range ->
        [from, to] = range |> String.split("-") |> Enum.map(&String.to_integer/1)
        MapSet.new(Range.new(from, to))
      end)
    end)
  end
end
