defmodule AdventOfCode.Y2017.Day02 do
  @moduledoc """
  --- Day 2: Corruption Checksum ---
  Problem Link: https://adventofcode.com/2017/day/2
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 2

  def run_1 do
    input!()
    |> parse()
    |> Enum.map(&Enum.min_max/1)
    |> Enum.map(fn {min, max} -> max - min end)
    |> Enum.sum()
  end

  def run_2 do
    input!()
    |> parse()
    |> Enum.map(&evenly_divide/1)
    |> Enum.map(fn [a, b] -> div(a, b) end)
    |> Enum.sum()
  end

  def parse(input) do
    input
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.split("\t")
      |> Enum.map(&String.to_integer/1)
    end)
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
end
