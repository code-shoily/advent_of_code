defmodule AdventOfCode.Y2015.Day3 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/3
  """
  use AdventOfCode.Data.InputReader, year: 2015, day: 3
  @start [0, 0]

  defp next_location("^", [x, y]), do: [x, y + 1]
  defp next_location("v", [x, y]), do: [x, y - 1]
  defp next_location(">", [x, y]), do: [x + 1, y]
  defp next_location("<", [x, y]), do: [x - 1, y]

  def deliver_present(<<direction::bytes-size(1)>> <> rest) do
    next = next_location(direction, @start)
    deliver_present(rest, next, MapSet.new([[0, 0], next]))
  end

  def deliver_present("", _, db), do: db

  def deliver_present(<<direction::bytes-size(1)>> <> rest, current, db) do
    next = next_location(direction, current)
    deliver_present(rest, next, MapSet.put(db, next))
  end

  def run_1 do
    input!() |> deliver_present() |> MapSet.size()
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
