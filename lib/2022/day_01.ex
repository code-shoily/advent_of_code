defmodule AdventOfCode.Y2022.Day01 do
  @moduledoc """
  --- Day 1: Calorie Counting ---
  Problem Link: https://adventofcode.com/2022/day/1
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

    # |> Enum.reduce({[], 0}, fn
    #   "", {calories, total} -> {[total | calories], 0}
    #   calorie, {calories, total} -> {calories, total + String.to_integer(calorie)}
    # end)
    # |> then(fn {calories, last_calorie} -> [last_calorie | calories] end)
  end
end
