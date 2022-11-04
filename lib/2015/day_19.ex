defmodule AdventOfCode.Y2015.Day19 do
  @moduledoc """
  --- Day 19: Medicine for Rudolph ---
  Problem Link: https://adventofcode.com/2015/day/19
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 19

  alias AdventOfCode.Helpers.Transformers

  def run(input \\ input!()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1({molecule, replacements}) do
    replacements
    |> Enum.map(&replace_by_one(molecule, &1))
    |> Enum.reduce(&MapSet.union/2)
    |> Enum.count()
  end

  defp run_2(_input) do
    {:todo, 2}
  end

  def parse(data \\ input!()) do
    [molecule | replacements] = Enum.reverse(Transformers.lines(data))
    replacements = parse_replacements(Enum.reverse(replacements))
    {molecule, replacements}
  end

  defp parse_replacements(replacements) do
    replacements
    |> Enum.map(fn replacement ->
      List.to_tuple(String.split(replacement, " => "))
    end)
  end

  def replace_by_one(molecule, {find, replace}) when is_binary(molecule) do
    case String.split(molecule, find) do
      [_] -> MapSet.new()

      matched ->
        idxs = Enum.into(Range.new(1, (length(matched) - 1) * 2, 2), [])

        replace_by_one(
          Enum.intersperse(matched, find),
          idxs,
          replace
        )
    end
  end

  def replace_by_one(matches, indices, replacement) do
    indices
    |> Enum.map(fn index ->
      matches
      |> List.replace_at(index, replacement)
      |> Enum.join("")
    end)
    |> Enum.into(%MapSet{})
  end
end
