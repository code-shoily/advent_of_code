defmodule AdventOfCode.Y2018.Day02 do
  @moduledoc """
  --- Day 2: Inventory Management System ---
  Problem Link: https://adventofcode.com/2018/day/2
  """

  use AdventOfCode.Helpers.InputReader, year: 2018, day: 2

  def run_1 do
    input!()
    |> parse()
    |> Enum.map(fn text ->
      text |> letter_count() |> two_or_three_count()
    end)
    |> checksum()
  end

  def run_2 do
    input!()
    |> parse()
    |> Enum.flat_map(&words_without_a_char/1)
    |> Enum.group_by(& &1)
    |> Enum.map(fn {box, boxes} -> {box, length(boxes)} end)
    |> Enum.filter(fn {_, freq} -> freq == 2 end)
    |> then(fn [{box, _}] -> String.replace(box, "?", "") end)
  end

  def parse(data), do: String.split(data, "\n")

  def letter_count(word) do
    word
    |> String.graphemes()
    |> Enum.group_by(& &1)
    |> Enum.map(fn {key, val} -> {key, length(val)} end)
    |> Map.new()
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
    |> Tuple.product()
  end

  def remove_at(str, idx) do
    str
    |> String.split_at(idx)
    |> then(fn {x, <<_>> <> xs} -> x <> "?" <> xs end)
  end

  def words_without_a_char(s) do
    Enum.map(0..(String.length(s) - 1), &remove_at(s, &1))
  end
end
