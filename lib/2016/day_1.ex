defmodule AdventOfCode.Y2016.Day1 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2016/day/1
  """
  use AdventOfCode.Data.InputReader, year: 2016, day: 1

  def process(input) do
    input
    |> String.trim()
    |> String.split(",")
    |> Enum.map(&String.trim/1)
  end

  def move(directions) do
    move(:origin, directions, [0, 0])
  end

  def move(_, [], position) do
    position
  end

  def move(:origin, ["L" <> steps | rest], [x, y]) do
    move(:west, rest, [x - String.to_integer(steps), y])
  end

  def move(:origin, ["R" <> steps | rest], [x, y]) do
    move(:east, rest, [x + String.to_integer(steps), y])
  end

  def move(:north, ["L" <> steps | rest], [x, y]) do
    move(:west, rest, [x - String.to_integer(steps), y])
  end

  def move(:north, ["R" <> steps | rest], [x, y]) do
    move(:east, rest, [x + String.to_integer(steps), y])
  end

  def move(:south, ["L" <> steps | rest], [x, y]) do
    move(:east, rest, [x + String.to_integer(steps), y])
  end

  def move(:south, ["R" <> steps | rest], [x, y]) do
    move(:west, rest, [x - String.to_integer(steps), y])
  end

  def move(:west, ["L" <> steps | rest], [x, y]) do
    move(:south, rest, [x, y - String.to_integer(steps)])
  end

  def move(:west, ["R" <> steps | rest], [x, y]) do
    move(:north, rest, [x, y + String.to_integer(steps)])
  end

  def move(:east, ["L" <> steps | rest], [x, y]) do
    move(:north, rest, [x, y + String.to_integer(steps)])
  end

  def move(:east, ["R" <> steps | rest], [x, y]) do
    move(:south, rest, [x, y - String.to_integer(steps)])
  end

  def blocks([x, y]) do
    abs(x) + abs(y)
  end

  def run_1 do
    input!()
    |> process()
    |> move()
    |> blocks()
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
