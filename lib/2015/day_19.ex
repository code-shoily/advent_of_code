defmodule AdventOfCode.Y2015.Day19 do
  @moduledoc """
  --- Day 19: Medicine for Rudolph ---
  Problem Link: https://adventofcode.com/2015/day/19
  Difficulty: m
  Tags: not-fast-enough revisit vector random-access

  Helpful Tips for Part II: (,) analogy
  https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4etju
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 19)

  def run(input \\ input()) do
    input = parse(input)

    {distinct_molecule_count(input), least_fabrication_steps(input)}
  end

  def parse(data \\ input()) do
    [molecule | replacements] = Enum.reverse(Transformers.lines(data))
    replacements = parse_replacements(Enum.reverse(replacements))
    {molecule, replacements}
  end

  defp parse_replacements(replacements) do
    Enum.map(replacements, &List.to_tuple(String.split(&1, " => ")))
  end

  defp distinct_molecule_count({molecule, replacements}) do
    replacements
    |> Enum.map(&replace_by_one(molecule, &1))
    |> Enum.reduce(&MapSet.union/2)
    |> Enum.count()
  end

  defp least_fabrication_steps({molecule, _}) do
    molecule = List.flatten(Regex.scan(~r/[A-Z][a-z]?/, molecule))
    len = Enum.count(molecule)
    parens = Enum.count(molecule, &(&1 == "Rn" || &1 == "Ar"))
    comma = Enum.count(molecule, &(&1 == "Y"))

    len - parens - 2 * comma - 1
  end

  defp replace_by_one(molecule, {find, replace}) do
    (Enum.count(match = String.split(molecule, find)) > 1 &&
       replace_by_one(Enum.intersperse(match, find), indices(match), replace)) ||
      MapSet.new()
  end

  defp replace_by_one(matches, indices, replacement) do
    for index <- indices, into: %MapSet{} do
      Enum.join(List.replace_at(matches, index, replacement), "")
    end
  end

  defp indices(matches) do
    for i <- Range.new(1, (length(matches) - 1) * 2, 2), do: i
  end
end
