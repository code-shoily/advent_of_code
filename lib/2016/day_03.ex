defmodule AdventOfCode.Y2016.Day03 do
  @moduledoc """
  --- Day 3: Squares With Three Sides ---
  Problem Link: https://adventofcode.com/2016/day/3
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @input InputReader.read_from_file(2016, 3)

  def run(input \\ @input) do
    {run_1(input), run_2(input)}
  end

  def run_1(input) do
    input
    |> Transformers.lines()
    |> Enum.map(&group/1)
    |> count_triangles()
  end

  def run_2(input), do: count_triangles(transpose_group(input))

  def triangle?([a, b, c]), do: a + b > c and b + c > a and c + a > b
  def count_triangles(triples), do: Enum.count(triples, &triangle?/1)

  def group(line) do
    line
    |> String.trim()
    |> String.split()
    |> Enum.map(&String.to_integer/1)
  end

  def transpose_group(input) do
    input
    |> Transformers.lines()
    |> Enum.flat_map(fn d -> String.split(d, ~r{\s}, trim: true) end)
    |> Enum.map(&String.to_integer/1)
    |> then(fn data ->
      [data, tl(data), tl(tl(data))]
      |> Enum.flat_map(&(&1 |> Enum.take_every(3) |> Enum.chunk_every(3)))
    end)
  end
end
