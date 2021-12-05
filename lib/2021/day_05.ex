defmodule AdventOfCode.Y2021.Day05 do
  @moduledoc """
  --- Day 5: Hydrothermal Venture ---
  Problem Link: https://adventofcode.com/2021/day/5
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 5

  def run_1, do: input!() |> parse() |> overlaps(false)
  def run_2, do: input!() |> parse() |> overlaps(true)
  def parse(data), do: Enum.map(String.split(data, "\n"), &ranges/1)

  defp ranges(line) do
    ~r/(\d+),(\d+) -> (\d+),(\d+)/
    |> Regex.run(line, capture: :all_but_first)
    |> Enum.map(&String.to_integer/1)
    |> Enum.split(2)
  end

  defp overlaps(ranges, diagonal?) do
    ranges
    |> Enum.flat_map(&points_between(&1, diagonal?))
    |> Enum.frequencies()
    |> Enum.count(&(elem(&1, 1) >= 2))
  end

  defp points_between({from, to}, diagonal?) do
    case {{from, to}, diagonal?} do
      {{[same, a], [same, b]}, _} -> Enum.map(a..b, &{same, &1})
      {{[a, same], [b, same]}, _} -> Enum.map(a..b, &{&1, same})
      {range, true} -> diagonals(range)
      _ -> []
    end
  end

  defp diagonals({[a, b], [c, d]}),
    do: Enum.map(0..abs(c - a), &{(a > c && a - &1) || a + &1, (b > d && b - &1) || b + &1})
end
