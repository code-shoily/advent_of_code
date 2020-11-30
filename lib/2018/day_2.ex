defmodule AdventOfCode.Y2018.Day2 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2018/day/2
  """

  use AdventOfCode.Helpers.InputReader, year: 2018, day: 2

  def process, do: String.split(input!(), "\n")

  def letter_count(word) do
    word
    |> String.graphemes()
    |> Enum.group_by(& &1)
    |> Enum.map(fn {k, v} -> {k, length(v)} end)
    |> Enum.into(%{})
  end

  def two_or_three_count(frequency) do
    frequency
    |> Enum.reduce({0, 0}, fn
      {_, 2}, {_, b} -> {1, b}
      {_, 3}, {a, _} -> {a, 1}
      _, acc -> acc
    end)
  end

  def checksum(two_or_threes) do
    two_or_threes
    |> Enum.reduce({0, 0}, fn {a, b}, {x, y} -> {a + x, b + y} end)
    |> (fn {a, b} -> a * b end).()
  end

  def run_1 do
    process()
    |> Enum.map(&letter_count/1)
    |> Enum.map(&two_or_three_count/1)
    |> checksum()
  end

  def remove_at(str, idx) do
    str
    |> String.split_at(idx)
    |> (fn {x, <<_>> <> xs} -> x <> "?" <> xs end).()
  end

  def words_without_a_char(s) do
    Enum.map(0..(String.length(s) - 1), &remove_at(s, &1))
  end

  def run_2 do
    process()
    |> Enum.flat_map(&words_without_a_char/1)
    |> Enum.group_by(& &1)
    |> Enum.map(fn {box, boxes} -> {box, length(boxes)} end)
    |> Enum.reject(fn {_, freq} -> freq != 2 end)
    |> (fn [{box, _}] -> String.replace(box, "?", "") end).()
  end

  def run, do: {run_1(), run_2()}
end
