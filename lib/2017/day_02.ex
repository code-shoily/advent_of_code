defmodule AdventOfCode.Y2017.Day02 do
  @moduledoc """
  --- Day 2: Corruption Checksum ---
  Problem Link: https://adventofcode.com/2017/day/2
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 2)

  def run(input \\ input()) do
    input = parse(input)

    {run(input, &Enum.min_max/1, &Kernel.-/2), run(input, &evenly_divide/1, &Kernel.div/2)}
  end

  def run(input, fn_1, fn_2) do
    Enum.reduce(input, 0, fn line, acc ->
      {min, max} = fn_1.(line)
      acc + fn_2.(max, min)
    end)
  end

  def parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(fn line ->
      line
      |> String.split("\t")
      |> Enum.map(&String.to_integer/1)
    end)
  end

  def evenly_divide(lst) do
    for i <- lst do
      for j <- lst do
        if i != j and i > j and rem(i, j) == 0 do
          {j, i}
        end
      end
      |> Enum.reject(&is_nil/1)
    end
    |> Enum.flat_map(& &1)
    |> List.first()
  end
end
