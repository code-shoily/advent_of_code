defmodule AdventOfCode.Y2015.Day21 do
  @moduledoc """
  --- Day 21: RPG Simulator 20XX ---
  Problem Link: https://adventofcode.com/2015/day/21
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 21

  alias AdventOfCode.Helpers.Transformers
  alias ExAlgo.Counting.Combinatorics

  @item_db ~S"""
  Weapons:    Cost  Damage  Armor
  Dagger        8     4       0
  Shortsword   10     5       0
  Warhammer    25     6       0
  Longsword    40     7       0
  Greataxe     74     8       0

  Armor:      Cost  Damage  Armor
  Leather      13     0       1
  Chainmail    31     0       2
  Splintmail   53     0       3
  Bandedmail   75     0       4
  Platemail   102     0       5

  Rings:      Cost  Damage  Armor
  Damage +1    25     1       0
  Damage +2    50     2       0
  Damage +3   100     3       0
  Defense +1   20     0       1
  Defense +2   40     0       2
  Defense +3   80     0       3
  """

  def run(input \\ input!()) do
    input = parse(input)

    {frugal_win(input), expensive_loss(input)}
  end

  defp frugal_win(boss) do
    find_determining_combination(store(), boss, &Enum.filter/2, &Enum.min_by/2)
  end

  defp expensive_loss(boss) do
    find_determining_combination(store(), boss, &Enum.reject/2, &Enum.max_by/2)
  end

  defp find_determining_combination(store, boss, filter, max_or_min) do
    store
    |> possible_stats()
    |> filter.(fn stat ->
      stat
      |> Map.merge(%{hit_points: 100})
      |> player_wins?(boss)
    end)
    |> max_or_min.(& &1.cost)
    |> then(& &1.cost)
  end

  def parse(data \\ input!()) do
    data
    |> Transformers.lines()
    |> Map.new(fn line ->
      [attr, pts] = String.split(line, ": ")

      {to_attr(attr), String.to_integer(pts)}
    end)
  end

  defp player_wins?(player, boss) do
    number_of_rounds(boss, player) <= number_of_rounds(player, boss)
  end

  defp number_of_rounds(defender, attacker) do
    score_deduction = max(attacker.damage - defender.armor, 1)
    div(defender.hit_points, score_deduction)
  end

  defp to_attr("Hit Points"), do: :hit_points
  defp to_attr("Damage"), do: :damage
  defp to_attr("Armor"), do: :armor
  defp to_attr("Rings"), do: :rings
  defp to_attr("Weapons"), do: :weapons

  def store do
    @item_db
    |> String.split("\n\n", trim: true)
    |> Enum.map(&Transformers.lines/1)
    |> Map.new(fn [title | data] ->
      {
        String.split(title, ": ") |> List.first() |> to_attr(),
        data
        |> Enum.map(fn line ->
          line
          |> Transformers.words()
          |> Enum.slice(-3, 3)
          |> parse_store_item()
        end)
      }
    end)
  end

  defp parse_store_item(values) do
    [cost, damage, armor] = Enum.map(values, &String.to_integer/1)
    %{cost: cost, damage: damage, armor: armor}
  end

  def possible_stats(store) do
    for weapons <- weapons_combinations(store),
        armor <- armor_combinations(store),
        rings <- rings_combinations(store) do
      [weapons, armor, rings]
      |> Enum.concat()
      |> Enum.reduce(&Map.merge(&1, &2, fn _, a, b -> a + b end))
    end
  end

  defp weapons_combinations(%{weapons: weapons}), do: combinations(weapons, 1, 1)
  defp armor_combinations(%{armor: armor}), do: combinations(armor, 0, 1)
  defp rings_combinations(%{rings: rings}), do: combinations(rings, 0, 2)

  defp combinations(item_store, lower, upper) do
    lower..upper
    |> Enum.flat_map(fn qty ->
      Combinatorics.combinations(item_store, qty)
    end)
  end
end
