defmodule AdventOfCode.Y2020.Day21 do
  @moduledoc """
  --- Day 21: Allergen Assessment ---
  Problem Link: https://adventofcode.com/2020/day/21
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 21)

  def run(input \\ input()) do
    input = parse(input)
    {allergen_free(input), input |> build() |> evolve() |> sort()}
  end

  def parse(input) do
    for line <- Transformers.lines(input) do
      decrypt(line)
    end
  end

  defp sort(allergens), do: Enum.map_join(allergens, ",", &hd(MapSet.to_list(elem(&1, 1))))
  defp ingredients(foods), do: Enum.flat_map(foods, &elem(&1, 1))

  defp allergen_free(foods) do
    allergens = allergens(foods)
    Enum.count(ingredients(foods), &(&1 not in allergens))
  end

  @regex ~r/^(?<items>.+) \(contains (?<allergens>.+)\)$/
  defp decrypt([_, m1, m2]), do: {String.split(m2, ", "), String.split(m1, " ")}
  defp decrypt(line), do: decrypt(Regex.run(@regex, line))

  defp build(foods) do
    foods
    |> Enum.reduce(%{}, fn {allergens, items}, acc ->
      items
      |> MapSet.new()
      |> then(fn items -> collect(allergens, acc, items) end)
    end)
    |> Map.new(fn {k, v} -> {k, intersections(v)} end)
  end

  defp collect(allergens, init, items) do
    Enum.reduce(allergens, init, fn allergen, acc ->
      Map.update(acc, allergen, [items], &[items | &1])
    end)
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
    |> Enum.map(fn {k, v} -> {k, (Enum.count(v) == 1 && v) || MapSet.difference(v, found)} end)
    |> Map.new()
    |> evolve(result)
  end
end
