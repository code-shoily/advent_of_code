defmodule AdventOfCode.Y2015.Day2 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/2
  """
  use AdventOfCode.Data.InputReader, year: 2015, day: 2

  @spec run_1 :: non_neg_integer()
  def run_1 do
    input!()
    |> process_input()
    |> Enum.map(fn n -> n |> Enum.map(&String.to_integer/1) end)
    |> Enum.map(&required_paper/1)
    |> Enum.sum()
  end

  defp process_input(lines) do
    lines
    |> String.split("\n")
    |> Enum.map(&String.split(&1, "x"))
  end

  @spec required_paper([non_neg_integer(), ...]) :: non_neg_integer()
  defp required_paper([_, _, _] = sides) do
    (sides |> surface_area()) + (sides |> area_per_dim() |> minimum())
  end

  @spec surface_area([non_neg_integer(), ...]) :: non_neg_integer()
  defp surface_area(sides) do
    sides
    |> area_per_dim()
    |> Enum.sum()
    |> Kernel.*(2)
  end

  @spec area_per_dim([non_neg_integer(), ...]) :: [non_neg_integer(), ...]
  defp area_per_dim([width, length, height]) do
    [length * width, width * height, height * length]
  end

  @spec minimum([non_neg_integer(), ...]) :: non_neg_integer()
  defp minimum([side_1, side_2, side_3]) do
    side_1 |> min(side_2) |> min(side_3)
  end

  @spec run_2 :: non_neg_integer()
  def run_2 do
    input!()
    |> process_input()
    |> Enum.map(fn n -> Enum.map(n, &String.to_integer/1) end)
    |> Enum.map(&smallest_perimeter_plus_volume/1)
    |> Enum.reduce(&(&1 + &2))
  end

  @spec smallest_perimeter_plus_volume([non_neg_integer(), ...]) :: non_neg_integer()
  defp smallest_perimeter_plus_volume([width, length, height]) do
    volume = width * length * height

    smallest_perimeter =
      [width + length, length + height, height + width]
      |> Enum.sort()
      |> hd()
      |> Kernel.*(2)

    volume + smallest_perimeter
  end

  def run, do: {run_1(), run_2()}
end
