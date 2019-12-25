defmodule AdventOfCode.Y2015.Day3 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/3
  """
  use AdventOfCode.Data.InputReader, year: 2015, day: 3
  def run, do: {run_1(), run_2()}

  def run_1 do
    input!() |> deliver_present() |> MapSet.size()
  end

  @start [0, 0]

  defp next_location("^", [x, y]), do: [x, y + 1]
  defp next_location("v", [x, y]), do: [x, y - 1]
  defp next_location(">", [x, y]), do: [x + 1, y]
  defp next_location("<", [x, y]), do: [x - 1, y]

  defp deliver_present(<<direction::bytes-size(1)>> <> rest) do
    next = next_location(direction, @start)
    deliver_present(rest, next, MapSet.new([[0, 0], next]))
  end

  defp deliver_present("", _, db), do: db

  defp deliver_present(<<direction::bytes-size(1)>> <> rest, current, db) do
    next = next_location(direction, current)
    deliver_present(rest, next, MapSet.put(db, next))
  end

  def run_2 do
    input!()
    |> String.graphemes()
    |> alternate()
    |> deliver_present_each()
    |> MapSet.size()
  end

  defp alternate(directions) do
    santa = directions |> Enum.take_every(2) |> Enum.join()
    robo_santa = directions |> tl() |> Enum.take_every(2) |> Enum.join()

    {santa, robo_santa}
  end

  defp deliver_present_each({santa, robo_santa}) do
    MapSet.union(
      deliver_present(santa),
      deliver_present(robo_santa)
    )
  end
end
