defmodule AdventOfCode.Y2016.Day06 do
  @moduledoc """
  --- Day 6: Signals and Noise ---
  Problem Link: https://adventofcode.com/2016/day/6
  """
  use AdventOfCode.Helpers.InputReader, year: 2016, day: 6

  def run_1, do: input!() |> parse() |> Enum.map_join(&frequency(&1, :max))
  def run_2, do: input!() |> parse() |> Enum.map_join(&frequency(&1, :min))

  def parse(input) do
    len = String.split(input, "\n", trim: true) |> hd() |> String.length()

    input
    |> String.replace("\n", "")
    |> String.codepoints()
    |> Enum.with_index()
    |> Enum.map(fn {k, idx} -> {k, rem(idx, len)} end)
    |> Enum.group_by(fn {_, group} -> group end, fn {v, _} -> v end)
    |> Map.values()
    |> Enum.map(&Enum.join/1)
  end

  defp frequency(word, by) do
    word
    |> String.codepoints()
    |> Enum.group_by(& &1)
    |> Map.values()
    |> Enum.min_max_by(&length/1)
    |> then(fn {min, max} -> hd((by == :max && max) || min) end)
  end
end
