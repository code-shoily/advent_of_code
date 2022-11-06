defmodule AdventOfCode.Y2015.Day13 do
  @moduledoc """
  --- Day 13: Knights of the Dinner Table ---
  Problem Link: https://adventofcode.com/2015/day/13
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 13

  alias ExAlgo.Counting.Combinatorics

  def run(input \\ input!()) do
    {people, facts} = parse(input)

    {
      maximize_happiness({people, facts}),
      happiness_with_me({people, facts})
    }
  end

  def parse(data) do
    facts =
      data
      |> String.split("\n", trim: true)
      |> Enum.map(&parse_fact/1)
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Enum.flat_map(fn {mapset, [happiness_1, happiness_2]} ->
        [a, b] = MapSet.to_list(mapset)
        happiness = happiness_1 + happiness_2

        [{{a, b}, happiness}, {{b, a}, happiness}]
      end)
      |> Enum.into(%{})

    people =
      facts
      |> Map.keys()
      |> Enum.flat_map(&Tuple.to_list/1)
      |> Enum.uniq()

    {people, facts}
  end

  @regex ~r/(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\./
  def parse_fact(statement) do
    @regex
    |> Regex.run(statement, capture: :all_but_first)
    |> then(fn [person_a, action, value, person_b] ->
      {
        MapSet.new([person_a, person_b]),
        String.to_integer(value) * ((action == "gain" && 1) || -1)
      }
    end)
  end

  defp happiness_with_me({people, facts}) do
    facts =
      people
      |> Enum.flat_map(fn person ->
        [
          {{"Me", person}, 0},
          {{person, "Me"}, 0}
        ]
      end)
      |> Enum.into(%{})
      |> Map.merge(facts)

    people = ["Me" | people]

    maximize_happiness({people, facts})
  end

  defp maximize_happiness({people, facts}) do
    people
    |> all_pairs()
    |> Enum.map(&collect_happiness(&1, facts))
    |> Enum.max()
  end

  defp all_pairs(people) do
    people
    |> Combinatorics.permutations()
    |> Enum.flat_map(fn [first | _] = people ->
      [people ++ [first]]
    end)
    |> Enum.map(&Enum.chunk_every(&1, 2, 1, :discard))
  end

  defp collect_happiness(data, facts) do
    Enum.reduce(data, 0, fn [person_1, person_2], happiness ->
      happiness + facts[{person_1, person_2}]
    end)
  end
end
