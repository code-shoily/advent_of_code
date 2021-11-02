defmodule AdventOfCode.Y2020.Day21 do
  @moduledoc """
  --- Day 21: Allergen Assessment ---
  Problem Link: https://adventofcode.com/2020/day/21
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 21

  def run_1, do: input!() |> parse() |> allergen_free()
  def run_2, do: input!() |> parse() |> build() |> evolve() |> sort()

  def parse(input) do
    input
    |> String.split("\n")
    |> Enum.map(&decrypt/1)
  end

  defp sort(allergens), do: Enum.map_join(allergens, ",", &hd(MapSet.to_list(elem(&1, 1))))
  defp ingredients(foods), do: Enum.flat_map(foods, &elem(&1, 1))

  defp allergen_free(foods) do
    allergens = allergens(foods)
    length(Enum.reject(ingredients(foods), &(&1 in allergens)))
  end

  @regex ~r/^(?<items>.+) \(contains (?<allergens>.+)\)$/
  defp decrypt([_, m1, m2]), do: {String.split(m2, ", "), String.split(m1, " ")}
  defp decrypt(line), do: decrypt(Regex.run(@regex, line))

  defp build(foods) do
    foods
    |> Enum.reduce(%{}, fn {allergens, items}, acc ->
      items
      |> MapSet.new()
      |> then(fn items ->
        Enum.reduce(allergens, acc, fn allergen, acc ->
          Map.update(acc, allergen, [items], &[items | &1])
        end)
      end)
    end)
    |> Map.map(fn {_, v} -> intersections(v) end)
  end

  defp allergens(foods), do: build(foods) |> Map.values() |> unions()
  defp intersections(items), do: Enum.reduce(items, &MapSet.intersection/2)
  defp unions(items), do: Enum.reduce(items, &MapSet.union/2)

  defp evolve(allergen), do: evolve(allergen, %{})
  defp evolve(allergen, result) when map_size(allergen) == map_size(result), do: result

  defp evolve(allergen, result) do
    result = Map.merge(result, Map.new(Enum.filter(allergen, &(Enum.count(elem(&1, 1)) == 1))))
    found = unions(Map.values(result))

    allergen
    |> Map.map(fn {_, v} -> (Enum.count(v) == 1 && v) || MapSet.difference(v, found) end)
    |> evolve(result)
  end
end
