defmodule AdventOfCode.Y2017.Day2 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2017/day/2
  """
  use AdventOfCode.Data.InputReader, year: 2017, day: 2

  def process(input) do
    input
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.split("\t")
      |> Enum.map(&String.to_integer/1)
    end)
  end

  def run_1 do
    input!()
    |> process()
    |> Enum.map(&Enum.min_max/1)
    |> Enum.map(fn {min, max} -> max - min end)
    |> Enum.sum()
  end

  def evenly_divide(lst) do
    for i <- lst do
      for j <- lst do
        if i != j and i > j and rem(i, j) == 0 do
          [i, j]
        end
      end
      |> Enum.reject(&is_nil/1)
    end
    |> Enum.flat_map(& &1)
    |> hd()
  end

  def run_2 do
    input!()
    |> process()
    |> Enum.map(&evenly_divide/1)
    |> Enum.map(fn [a, b] -> div(a, b) end)
    |> Enum.sum()
  end

  def run, do: {run_1(), run_2()}
end
