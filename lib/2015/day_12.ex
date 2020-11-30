defmodule AdventOfCode.Y2015.Day12 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/12
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 12

  @numbers "-0123456789"
  @delim " "

  def number?(s), do: String.contains?(@numbers, s)

  def parse([], result), do: result

  def parse([h | t], []) do
    (number?(h) && parse(t, [h])) || parse(t, [@delim])
  end

  def parse([h | t], [x | xs] = res) do
    (number?(h) && parse(t, [x <> h | xs])) || parse(t, [@delim | res])
  end

  def compute(parsed) do
    parsed
    |> Enum.join()
    |> String.split()
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
  end

  def process do
    input!()
    |> String.graphemes()
  end

  def run_1 do
    process()
    |> parse([])
    |> compute()
  end

  def total(obj) when is_map(obj) do
    nodes = Map.values(obj)
    ("red" in nodes && 0) || total(nodes)
  end

  def total(obj) when is_list(obj) do
    obj
    |> Enum.map(fn
      value when is_map(value) or is_list(value) -> total(value)
      value when is_integer(value) -> value
      _ -> 0
    end)
    |> Enum.sum()
  end

  def run_2 do
    process()
    |> Jason.decode!()
    |> total()
  end

  def run, do: {run_1(), run_2()}
end
