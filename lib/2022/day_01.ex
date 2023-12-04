defmodule AdventOfCode.Y2022.Day01 do
  @moduledoc """
  --- Day 1: Calorie Counting ---
  Problem Link: https://adventofcode.com/2022/day/1
  Difficulty: xs
  Tags: sequence
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 1)

  def run(input \\ input()) do
    calories = parse(input)

    {
      Enum.max(calories),
      calories |> Enum.sort(:desc) |> Enum.take(3) |> Enum.sum()
    }
  end

  def parse(data) do
    data
    |> String.split(~r{(\r\n\r\n|\r\r|\n\n)}, trim: true)
    |> Enum.map(fn calories ->
      calories
      |> Transformers.lines()
      |> Enum.map(&String.to_integer/1)
      |> Enum.sum()
    end)
  end
end
