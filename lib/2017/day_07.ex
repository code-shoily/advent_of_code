defmodule AdventOfCode.Y2017.Day07 do
  @moduledoc """
  --- Day 7: Recursive Circus ---
  Problem Link: https://adventofcode.com/2017/day/7
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 7

  alias AdventOfCode.Helpers.Transformers

  def run(input \\ input!()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    input
    |> Enum.map(fn %{name: name} -> name end)
    |> MapSet.new()
    |> MapSet.difference(get_branches(input))
    |> MapSet.to_list()
    |> hd()
  end

  defp run_2(_input) do
    {:todo, 2}
  end

  @regex ~r/(?<name>[a-z]+) \((?<weight>\d+)\)( -> (?<branches>(\s*[a-z]+\,?)+))?/
  def parse(data \\ input!()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      Regex.named_captures(@regex, line)
    end)
    |> Enum.map(fn %{"branches" => branches, "weight" => weight, "name" => name} ->
      %{
        branches: (String.trim(branches) != "" && String.split(branches, ", ")) || nil,
        name: name,
        weight: String.to_integer(weight)
      }
    end)
  end

  defp get_branches(parsed_data) do
    parsed_data
    |> Enum.flat_map(fn
      %{branches: nil} -> []
      %{branches: branches} -> branches
    end)
    |> MapSet.new()
  end
end
