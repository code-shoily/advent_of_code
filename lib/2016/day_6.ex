defmodule AdventOfCode.Y2016.Day6 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2016/day/6
  """
  use AdventOfCode.Data.InputReader, year: 2016, day: 6

  def process(input) do
    len = String.split(input, "\n", trim: true) |> hd() |> String.length()

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
    input!() |> process() |> Enum.map_join(&frequency(&1, :max))
  end

  defp frequency(word, by) do
    word
    |> String.codepoints()
    |> Enum.group_by(& &1)
    |> Map.values()
    |> Enum.min_max_by(&length/1)
    |> (fn {min, max} -> hd((by == :max && max) || min) end).()
  end

  def run_2 do
    input!()
    |> process()
    |> Enum.map_join(&frequency(&1, :min))
  end

  def run, do: {run_1(), run_2()}
end
