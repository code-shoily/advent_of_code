defmodule AdventOfCode.Y2022.Day01 do
  @moduledoc """
  --- Day 1: Calorie Counting ---
  Problem Link: https://adventofcode.com/2022/day/1
  """
  def input, do: AdventOfCode.Helpers.InputReader.read_from_file(2022, 1)

  def run(input \\ input()) do
    calories = parse(input)

    {Enum.max(calories), calories |> Enum.sort() |> Enum.reverse() |> Enum.take(3) |> Enum.sum()}
  end

  def parse(data) do
    data
    |> String.split("\n")
    |> Enum.reduce({[], 0}, fn
      "", {calories, total} -> {[total | calories], 0}
      calorie, {calories, total} -> {calories, total + String.to_integer(calorie)}
    end)
    |> then(fn {calories, last_calorie} -> [last_calorie | calories] end)
  end
end
