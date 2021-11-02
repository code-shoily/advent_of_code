defmodule AdventOfCode.Y2019.Day01 do
  @moduledoc """
  --- Day 1: The Tyranny of the Rocket Equation ---
  Problem Link: https://adventofcode.com/2019/day/1
  """
  use AdventOfCode.Helpers.InputReader, year: 2019, day: 1

  @spec run_1 :: non_neg_integer()
  def run_1 do
    input!()
    |> parse()
    |> Enum.map(&fuel_required/1)
    |> Enum.sum()
  end

  @spec run_2 :: non_neg_integer()
  def run_2 do
    input!()
    |> parse()
    |> Enum.map(fn mass ->
      required_fuel = fuel_required(mass)
      refuel(required_fuel, required_fuel)
    end)
    |> Enum.sum()
  end

  def parse(data) do
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
