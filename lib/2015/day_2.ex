defmodule AdventOfCode.Y2015.Day2 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/2
  """
  use AdventOfCode.Data.InputReader, year: 2015, day: 2

  def surface_area(sides) do
    sides
    |> area_per_dim()
    |> Enum.sum()
    |> Kernel.*(2)
  end

  def area_per_dim([width, length, height]) do
    [length * width, width * height, height * length]
  end

  def minimum([side_1, side_2, side_3]) do
    side_1 |> min(side_2) |> min(side_3)
  end

  def required_paper([_, _, _] = sides) do
    (sides |> surface_area()) + (sides |> area_per_dim() |> minimum())
  end

  defp parse_input(lines) do
    lines
    |> String.split("\n")
    |> Enum.map(&String.split(&1, "x"))
  end

  def run do
    input!()
    |> parse_input()
    |> Enum.map(fn n -> n |> Enum.map(&String.to_integer/1) end)
    |> Enum.map(&required_paper/1)
    |> Enum.sum()
  end
end
