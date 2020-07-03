defmodule AdventOfCode.Y2016.Day6 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2016/day/6
  """
  use AdventOfCode.Data.InputReader, year: 2016, day: 6

  def process(input) do
    len = String.split(input, "\n", trim: true) |> Enum.at(0) |> String.length()

    input
    |> String.replace("\n", "")
    |> String.graphemes()
    |> Enum.with_index()
    |> Enum.map(fn {v, idx} -> {v, rem(idx, len)} end)
    |> Enum.group_by(fn {_, group} -> group end, fn {v, _} -> v end)
    |> Map.values()
    |> Enum.map(&Enum.join/1)
  end

  def run_1 do
    input!()
    |> process()
    |> Enum.map(&most_frequent_character/1)
    |> Enum.join()
  end

  defp most_frequent_character(word) do
    word
    |> String.codepoints()
    |> Enum.group_by(& &1)
    |> Map.values()
    |> Enum.max_by(&length/1)
    |> hd()
  end

  def run_2 do
    input!()
    |> process()
    |> Enum.map(&least_frequent_character/1)
    |> Enum.join()
  end

  defp least_frequent_character(word) do
    word
    |> String.codepoints()
    |> Enum.group_by(& &1)
    |> Map.values()
    |> Enum.min_by(&length/1)
    |> hd()
  end

  def run, do: {run_1(), run_2()}
end
