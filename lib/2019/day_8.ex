defmodule AdventOfCode.Y2019.Day8 do
  @moduledoc """
  Problem description: https://adventofcode.com/2019/day/8
  """
  use AdventOfCode.Data.InputReader, year: 2019, day: 8

  def run_1 do
    process()
    |> Enum.map(fn layer -> Enum.flat_map(layer, & &1) end)
    |> Enum.min_by(&Enum.count(&1, fn x -> x == 0 end))
    |> (fn x -> Enum.count(x, &(&1 == 2)) * Enum.count(x, &(&1 == 1)) end).()
  end

  def run_2 do
    2
  end

  def run do
    %{problem_1: run_1(), problem_2: run_2()}
  end

  def count_digits(xs, x) do
    Enum.reduce(xs, 0, &((&1 == x && &2 + 1) || &2))
  end

  def process() do
    input!()
    |> String.codepoints()
    |> Enum.map(&String.to_integer/1)
    |> Enum.chunk_every(25 * 6)
    |> Enum.map(&Enum.chunk_every(&1, 25))
  end
end
