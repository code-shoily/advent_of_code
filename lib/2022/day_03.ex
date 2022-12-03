defmodule AdventOfCode.Y2022.Day03 do
  @moduledoc """
  --- Day 3: Rucksack Reorganization ---
  Problem Link: https://adventofcode.com/2022/day/3
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 3)

  def run(input \\ input()) do
    rucksacks = Transformers.lines(input)

    {total_priority(compartment_priorities(rucksacks)),
     total_priority(group_priorities(rucksacks))}
  end

  defp compartment_priorities(rucksacks),
    do: Enum.map(rucksacks, &in_both_compartments/1)

  defp group_priorities(rucksacks) do
    rucksacks
    |> Enum.chunk_every(3)
    |> Enum.map(fn [x | xs] ->
      Enum.reduce(xs, as_set(x), &MapSet.intersection(as_set(&1), &2))
    end)
  end

  defp in_both_compartments(rucksack) do
    mid = rucksack |> String.length() |> div(2)

    [String.slice(rucksack, 0, mid), String.slice(rucksack, mid, mid)]
    |> Enum.map(&MapSet.new(String.codepoints(&1)))
    |> then(fn [a, b] -> MapSet.intersection(a, b) end)
  end

  defp as_set(rucksack), do: MapSet.new(String.codepoints(rucksack))

  defp priority(char) do
    case :binary.first(char) do
      lower when lower >= ?a and lower <= ?z -> lower - ?a + 1
      upper when upper >= ?A and upper <= ?Z -> upper - ?A + 27
    end
  end

  defp total_priority(common_items) do
    Enum.reduce(common_items, 0, fn x, acc ->
      [common_item] = MapSet.to_list(x)
      acc + priority(common_item)
    end)
  end
end
