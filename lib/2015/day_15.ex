defmodule AdventOfCode.Y2015.Day15 do
  @moduledoc """
  --- Day 15: Science for Hungry People ---
  Problem Link: https://adventofcode.com/2015/day/15
  FIXME: I hate this solution, I don't want to be hardwiring the total number of ingredients here.
  Should look into better algorithm to generate the quadruples (n-tuples really)
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 15)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  def run_1([sprinkles, peanut_butter, frosting, sugar]) do
    for {sprinkles_count, peanut_butter_count, frosting_count, sugar_count} <- get_quadruples() do
      {scoring_nutrients, _} =
        total_nutrition([
          nutrition_value(sprinkles, sprinkles_count),
          nutrition_value(peanut_butter, peanut_butter_count),
          nutrition_value(frosting, frosting_count),
          nutrition_value(sugar, sugar_count)
        ])

      scoring_nutrients |> Enum.product()
    end
    |> Enum.max()
  end

  def run_2([sprinkles, peanut_butter, frosting, sugar]) do
    for {sprinkles_count, peanut_butter_count, frosting_count, sugar_count} <- get_quadruples() do
      {scoring_nutrients, calories} =
        total_nutrition([
          nutrition_value(sprinkles, sprinkles_count),
          nutrition_value(peanut_butter, peanut_butter_count),
          nutrition_value(frosting, frosting_count),
          nutrition_value(sugar, sugar_count)
        ])

      (calories == 500 && Enum.product(scoring_nutrients)) || nil
    end
    |> Enum.reject(&is_nil/1)
    |> Enum.max()
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(&parse_nutrients/1)
  end

  @regex ~r{(?<name>\w+): capacity (?<capacity>\-?\d+), durability (?<durability>\-?\d+), flavor (?<flavor>\-?\d+), texture (?<texture>\-?\d+), calories (?<calories>\-?\d+)}
  defp parse_nutrients(ingredient) do
    @regex
    |> Regex.named_captures(ingredient)
    |> Map.delete("name")
    |> Map.new(fn {k, v} -> {String.to_atom(k), String.to_integer(v)} end)
  end

  defp nutrition_value(nutritions, spoon) do
    Map.new(nutritions, fn {k, v} -> {k, v * spoon} end)
  end

  defp total_nutrition([_, _, _, _] = nutrients) do
    capacity = nutrients |> total_of(:capacity)
    durability = nutrients |> total_of(:durability)
    flavor = nutrients |> total_of(:flavor)
    texture = nutrients |> total_of(:texture)
    calories = nutrients |> total_of(:calories)

    {[capacity, durability, flavor, texture], calories}
  end

  def total_of(nutrients, type) do
    nutrients
    |> Enum.map(& &1[type])
    |> Enum.sum()
    |> then(&((&1 < 0 && 0) || &1))
  end

  defp get_quadruples do
    for a <- 0..100,
        b <- 0..(100 - a),
        c <- 0..(100 - a - b),
        d <- 0..(100 - a - b - c),
        a + b + c + d == 100 do
      {a, b, c, d}
    end
  end
end
