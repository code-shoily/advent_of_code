defmodule AdventOfCode.Y2016.Day3 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2016/day/3
  """
  use AdventOfCode.Data.InputReader, year: 2016, day: 3

  def triangle?({a, b, c}) do
    a + b > c and b + c > a and c + a > b
  end

  def parse(line) do
    line
    |> String.trim()
    |> String.split()
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end

  def run_1 do
    input!()
    |> String.split("\n")
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&parse/1)
    |> Enum.filter(&triangle?/1)
    |> length
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
