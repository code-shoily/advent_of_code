defmodule AdventOfCode.Y2021.Day06 do
  @moduledoc """
  --- Day 6: Lanternfish ---
  Problem Link: https://adventofcode.com/2021/day/6
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 6

  def run_1, do: input!() |> parse() |> multiply(80) |> Map.values() |> Enum.sum()
  def run_2, do: input!() |> parse() |> multiply(256) |> Map.values() |> Enum.sum()

  def parse(data),
    do: data |> String.split(",") |> Enum.map(&String.to_integer/1) |> Enum.frequencies()

  def multiply(fishes, 0), do: fishes

  def multiply(fishes, day) do
    fishes
    |> Map.pop(0)
    |> then(fn {reset, fishes} ->
      fishes
      |> Enum.map(fn {k, v} -> {k - 1, v} end)
      |> Enum.into(%{})
      |> Map.merge(%{6 => reset, 8 => reset}, fn
        _, nil, v -> v
        _, v, nil -> v
        _, v1, v2 -> v1 + v2
      end)
      |> multiply(day - 1)
    end)
  end
end
