defmodule AdventOfCode.Y2020.Day01 do
  @moduledoc """
  --- Day 1: Report Repair ---
  Problem Link: https://adventofcode.com/2020/day/1
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 1

  def run_1, do: input!() |> parse() |> two_entries()
  def run_2, do: input!() |> parse() |> three_entries()

  def parse(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer(&1))
  end

  def two_entries(xs), do: do_two_entries(xs, MapSet.new(xs))

  def three_entries(xs), do: do_three_entries(xs, MapSet.new(xs))

  defp do_two_entries(xs, set) do
    Enum.reduce_while(xs, nil, fn x, _ ->
      halt_and_get(set, x, x)
    end)
  end

  defp do_three_entries([_ | ys] = xs, set) do
    Enum.reduce_while(xs, nil, fn x, _ ->
      Enum.reduce_while(ys, nil, fn y, _ ->
        halt_and_get(set, x + y, x * y)
      end)
      |> halt_and_get()
    end)
  end

  @year 2020
  defp halt_and_get(set, added, multiplied) do
    ((@year - added) in set &&
       {:halt, multiplied * (@year - added)}) ||
      {:cont, nil}
  end

  defp halt_and_get(nil), do: {:cont, nil}
  defp halt_and_get(result), do: {:halt, result}
end
