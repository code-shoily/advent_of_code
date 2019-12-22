defmodule AdventOfCode.Y2019.Day1 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2019/day/1
  """
  use AdventOfCode.Data.InputReader, year: 2019, day: 1

  def run, do: {run_1(), run_2()}

  @spec run_1 :: non_neg_integer()
  def run_1 do
    input!()
    |> process()
    |> Stream.map(&fuel_required/1)
    |> Enum.sum()
  end

  @spec run_2 :: non_neg_integer()
  def run_2 do
    input!()
    |> process()
    |> Stream.map(&fuel_required/1)
    |> Stream.map(&refuel(&1, &1))
    |> Enum.sum()
  end

  defp process(data) do
    data
    |> String.split("\n")
    |> Enum.map(&String.to_integer/1)
  end

  defp fuel_required(mass), do: Integer.floor_div(mass, 3) - 2

  defp refuel(fuel, total) do
    next = fuel_required(fuel)
    (next < 1 && total) || refuel(next, total + next)
  end
end
