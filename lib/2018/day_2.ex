defmodule AdventOfCode.Y2018.Day2 do
  use AdventOfCode.Data.InputReader, year: 2018, day: 2

  def process() do
    input!()
    |> String.split("\n", trim: true)
  end

  def word_count(word) do
    word
    |> String.graphemes()
    |> Enum.group_by(& &1)
    |> Enum.map(fn {k, v} -> {k, length(v)} end)
    |> Enum.into(%{})
  end

  @init %{two: 0, three: 0}
  def two_or_three_count(frequency) do
    frequency
    |> Enum.reduce(@init, fn {_, frequency}, acc ->
      case frequency do
        2 -> %{acc | two: 1}
        3 -> %{acc | three: 1}
        _ -> acc
      end
    end)
  end

  def checksum_1(two_or_threes) do
    two_or_threes
    |> Enum.reduce(%{two: 0, three: 0}, fn %{two: two, three: three}, acc ->
      acc
      |> Map.update(:two, 1, &(&1 + two))
      |> Map.update(:three, 1, &(&1 + three))
    end)
    |> (&(&1[:two] * &1[:three])).()
  end

  def run_1 do
    process()
    |> Enum.map(&word_count/1)
    |> Enum.map(&two_or_three_count/1)
    |> checksum_1()
  end

  def run_2, do: process()

  def run, do: {run_1(), run_2()}
end
