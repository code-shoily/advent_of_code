defmodule AdventOfCode.Y2017.Day4 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2017/day/4
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 4

  def run_1, do: input!() |> process() |> count_valids(&duplicates?/1)
  def run_2, do: input!() |> process() |> count_valids(&anagrams?/1)
  def run, do: {run_1(), run_2()}

  def process(input), do: String.split(input, "\n") |> Enum.map(&String.split(&1, " "))

  def count_valids(data, fun), do: data |> Enum.map(fun) |> Enum.filter(& &1) |> length()

  def duplicates?(words) do
    words
    |> Enum.reduce_while(%MapSet{}, fn word, set ->
      (word in set && {:halt, nil}) || {:cont, MapSet.put(set, word)}
    end)
    |> then(&(!is_nil(&1)))
  end

  def anagrams?(words) do
    words
    |> Enum.map(fn str -> str |> String.graphemes() |> Enum.sort() end)
    |> duplicates?()
  end
end
