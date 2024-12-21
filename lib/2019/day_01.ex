defmodule AdventOfCode.Y2019.Day01 do
  @moduledoc """
  --- Day 1: The Tyranny of the Rocket Equation ---
  Problem Link: https://adventofcode.com/2019/day/1
  Difficulty: xs
  Tags: calculation rust
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2019, 1)

  def run(input \\ input()) do
    parsed_data = parse(input)
    {run_1(parsed_data), run_2(parsed_data)}
  end

  def run_1(input) do
    Enum.sum_by(input, &fuel_required/1)
  end

  def run_2(input) do
    input
    |> Enum.sum_by(fn mass ->
      required_fuel = fuel_required(mass)
      refuel(required_fuel, required_fuel)
    end)
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.to_integer/1)
  end

  defp fuel_required(mass), do: Integer.floor_div(mass, 3) - 2

  defp refuel(fuel, total) do
    next = fuel_required(fuel)
    (next < 1 && total) || refuel(next, total + next)
  end
end
