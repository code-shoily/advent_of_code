defmodule AdventOfCode.Y2021.Day01 do
  @moduledoc """
  --- Day 1: Sonar Sweep ---
  Problem Link: https://adventofcode.com/2021/day/1
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 1

  def run_1, do: input!() |> parse() |> depth_increase()
  def run_2, do: input!() |> parse() |> sliding_window() |> depth_increase()

  def parse(data) do
    data
    |> String.split("\n")
    |> Enum.map(&String.to_integer/1)
  end

  defp depth_increase(measurements) do
    measurements
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.count(fn [a, b] -> b - a > 0 end)
  end

  defp sliding_window(measurements) do
    measurements
    |> Enum.chunk_every(3, 1, :discard)
    |> Enum.map(&Enum.sum/1)
  end
end
