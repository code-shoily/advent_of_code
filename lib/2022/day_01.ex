defmodule AdventOfCode.Y2022.Day01 do
  @moduledoc """
  --- Day 1: Calorie Counting ---
  Problem Link: https://adventofcode.com/2022/day/1
  """
  @input AdventOfCode.Helpers.InputReader.read_from_file(2022, 1)

  def run(input \\ @input) do
    {calorie_set, _} = parse(input)
    {_, top_3} = top_n_calories(calorie_set, 3)

    {:gb_sets.largest(calorie_set), Enum.sum(top_3)}
  end

  def parse(data \\ @input) do
    Enum.reduce(String.split(data, "\n"), {:gb_sets.new(), 0}, fn
      "", {calorie_set, total} -> {add_calories(calorie_set, total), 0}
      calorie, {calorie_set, total} -> {calorie_set, String.to_integer(calorie) + total}
    end)
  end

  defp add_calories(set, value),
    do: (:gb_sets.is_element(value, set) && set) || :gb_sets.insert(value, set)

  defp top_n_calories(calories, limit) do
    Enum.reduce(1..limit, {calories, []}, fn _, {sets, xs} ->
      {largest, sets} = :gb_sets.take_largest(sets)
      {sets, [largest | xs]}
    end)
  end
end
