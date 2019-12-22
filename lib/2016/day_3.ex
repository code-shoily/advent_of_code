defmodule AdventOfCode.Y2016.Day3 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2016/day/3
  """
  use AdventOfCode.Data.InputReader, year: 2016, day: 3

  def triangle?([a, b, c]) do
    a + b > c and b + c > a and c + a > b
  end

  def group(line) do
    line
    |> String.trim()
    |> String.split()
    |> Enum.map(&String.to_integer/1)
  end

  def run_1 do
    input!()
    |> String.split("\n")
    |> Enum.map(&group/1)
    |> Enum.filter(&triangle?/1)
    |> length
  end

  def transpose_group() do
    input!()
    |> String.split("\n")
    |> Enum.flat_map(fn d -> String.split(d, ~r{\s}, trim: true) end)
    |> Enum.map(&String.to_integer/1)
    |> (fn data ->
          [data, tl(data), tl(tl(data))]
          |> Enum.flat_map(&(&1 |> Enum.take_every(3) |> Enum.chunk_every(3)))
        end).()
  end

  def run_2 do
    transpose_group() |> Enum.filter(&triangle?/1) |> length
  end

  def run, do: {run_1(), run_2()}
end
