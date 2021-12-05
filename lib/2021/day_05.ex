defmodule AdventOfCode.Y2021.Day05 do
  @moduledoc """
  --- Day 5: Hydrothermal Venture ---
  Problem Link: https://adventofcode.com/2021/day/5
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 5

  def run_1, do: input!() |> parse() |> Enum.filter(&colinear?/1) |> overlaps()
  def run_2, do: input!() |> parse() |> overlaps()
  def parse(data), do: data |> String.split("\n") |> Enum.map(&ranges/1)

  defp ranges(line) do
    line
    |> String.split(" -> ")
    |> then(fn [from, to] ->
      {Enum.map(String.split(from, ","), &String.to_integer/1),
       Enum.map(String.split(to, ","), &String.to_integer/1)}
    end)
  end

  defp overlaps(ranges) do
    ranges
    |> Enum.flat_map(&points_between/1)
    |> Enum.frequencies()
    |> Enum.count(fn {_, value} -> value >= 2 end)
  end

  defp colinear?({[a, _], [a, _]}), do: true
  defp colinear?({[_, b], [_, b]}), do: true
  defp colinear?(_), do: false

  defp points_between({from, to}) do
    case {from, to} do
      {[a, b], [c, d]} when abs(c - a) == abs(d - b) ->
        Enum.map(0..abs(c - a), fn x ->
          {(a > c && a - x) || a + x, (b > d && b - x) || b + x}
        end)

      {[a, b], [a, c]} ->
        b..c |> Enum.map(fn x -> {a, x} end)

      {[a, b], [c, b]} ->
        a..c |> Enum.map(fn x -> {x, b} end)
    end
  end
end
