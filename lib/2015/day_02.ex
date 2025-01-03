defmodule AdventOfCode.Y2015.Day02 do
  @moduledoc """
  --- Day 2: I Was Told There Would Be No Math ---
  Problem Link: https://adventofcode.com/2015/day/2
  Difficulty: xs
  Tags: geometry2d rust
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 2)

  def run(input \\ input()) do
    data = parse(input)

    {run_1(data), run_2(data)}
  end

  def parse(input \\ input()) do
    input
    |> Transformers.lines()
    |> Enum.map(&Transformers.words(&1, "x"))
  end

  defp run_1(input) do
    Enum.sum_by(input, fn n -> n |> Enum.map(&String.to_integer/1) |> required_paper() end)
  end

  defp run_2(input) do
    Enum.sum_by(input, fn n ->
      Enum.map(n, &String.to_integer/1) |> smallest_perimeter_plus_volume()
    end)
  end

  @spec required_paper([pos_integer(), ...]) :: pos_integer()
  defp required_paper(sides) do
    surface_area(sides) + minimum(area_per_dim(sides))
  end

  @spec surface_area([pos_integer(), ...]) :: pos_integer()
  defp surface_area(sides) do
    sides
    |> area_per_dim()
    |> Enum.sum()
    |> Kernel.*(2)
  end

  @spec area_per_dim([pos_integer(), ...]) :: [pos_integer(), ...]
  defp area_per_dim([width, length, height]) do
    [length * width, width * height, height * length]
  end

  @spec minimum([pos_integer(), ...]) :: pos_integer()
  defp minimum([side_1, side_2, side_3]) do
    side_1 |> min(side_2) |> min(side_3)
  end

  @spec smallest_perimeter_plus_volume([pos_integer(), ...]) :: pos_integer()
  defp smallest_perimeter_plus_volume([width, length, height]) do
    [width + length, length + height, height + width]
    |> Enum.min()
    |> Kernel.*(2)
    |> Kernel.+(width * length * height)
  end
end
