defmodule AdventOfCode.Y2017.Day04 do
  @moduledoc """
  --- Day 4: High-Entropy Passphrases ---
  Problem Link: https://adventofcode.com/2017/day/4
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 4)

  def run(input \\ input()) do
    input = input |> Transformers.lines() |> Enum.map(&Transformers.words/1)

    {run_1(input), run_2(input)}
  end

  def run_1(input), do: count_valids(input, &duplicates?/1)
  def run_2(input), do: count_valids(input, &anagrams?/1)

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
