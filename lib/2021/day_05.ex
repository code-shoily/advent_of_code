defmodule AdventOfCode.Y2021.Day05 do
  @moduledoc """
  --- Day 5: Hydrothermal Venture ---
  Problem Link: https://adventofcode.com/2021/day/5
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 5

  def run_1, do: input!() |> parse() |> overlaps()
  def run_2, do: input!() |> parse() |> overlaps(true)
  def parse(data), do: Enum.map(String.split(data, "\n"), fn line -> ranges(line) end)

  defp ranges(line) do
    line
    |> String.split(" -> ")
    |> then(fn [from, to] ->
      {Enum.map(String.split(from, ","), &String.to_integer/1),
       Enum.map(String.split(to, ","), &String.to_integer/1)}
    end)
  end

  defp overlaps(ranges, diagonal? \\ false) do
    ranges
    |> Enum.flat_map(&points_between(&1, diagonal?))
    |> Enum.frequencies()
    |> Enum.count(&(elem(&1, 1) >= 2))
  end

  defp points_between({from, to}, diagonal?) do
    case {diagonal?, {from, to}} do
      {_, {[a, b], [a, c]}} ->
        Enum.map(b..c, &{a, &1})

      {_, {[a, b], [c, b]}} ->
        Enum.map(a..c, &{&1, b})

      {true, {[a, b], [c, d]}} ->
        Enum.map(0..abs(c - a), &{(a > c && a - &1) || a + &1, (b > d && b - &1) || b + &1})

      _ ->
        []
    end
  end
end
