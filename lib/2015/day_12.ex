defmodule AdventOfCode.Y2015.Day12 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/12
  """
  use AdventOfCode.Data.InputReader, year: 2015, day: 12

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

  def run_1 do
    input!()
    |> String.graphemes()
    |> parse([])
    |> compute()
  end

  def run_2 do
    0
  end

  @spec run :: %{
          problem_1: integer(),
          problem_2: integer()
        }
  def run do
    %{
      problem_1: run_1(),
      problem_2: run_2()
    }
  end
end
