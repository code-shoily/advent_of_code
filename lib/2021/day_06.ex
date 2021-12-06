defmodule AdventOfCode.Y2021.Day06 do
  @moduledoc """
  --- Day 6: Lanternfish ---
  Problem Link: https://adventofcode.com/2021/day/6
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 6

  def run_1, do: input!() |> parse() |> multiply(80) |> Enum.sum()
  def run_2, do: input!() |> parse() |> multiply(256) |> Enum.sum()

  def parse(data),
    do: data |> String.split(",") |> Enum.map(&String.to_integer/1) |> Enum.frequencies()

  def multiply(fishes, day) do
    (day == 0 && Map.values(fishes)) ||
      fishes
      |> Map.pop(0)
      |> then(fn {z, fishes} ->
        Map.merge(
          for({k, v} <- fishes, into: %{}, do: {k - 1, v}),
          %{6 => z || 0, 8 => z || 0},
          fn _, a, b -> a + b end
        )
      end)
      |> multiply(day - 1)
  end
end
