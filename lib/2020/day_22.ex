defmodule AdventOfCode.Y2020.Day22 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/22
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 22

  def run_1, do: input!() |> process() |> play() |> score()
  def run_2, do: {:not_implemented, 2}

  def process(input) do
    input
    |> String.split("\n\n")
    |> Enum.flat_map(&String.split(&1, ":"))
    |> decks()
  end

  defp decks([_, player_1, _, player_2]), do: {deck(player_1), deck(player_2)}
  defp deck(player), do: Enum.map(String.split(player, "\n", trim: true), &String.to_integer/1)

  defp play({[], player_2}), do: player_2
  defp play({player_1, []}), do: player_1

  defp play({[card_1 | player_1], [card_2 | player_2]}) when card_1 > card_2,
    do: play({player_1 ++ [card_1, card_2], player_2})

  defp play({[card_1 | player_1], [card_2 | player_2]}),
    do: play({player_1, player_2 ++ [card_2, card_1]})

  def score(cards) do
    cards
    |> Enum.reverse()
    |> Enum.with_index(1)
    |> Enum.map(fn {val, idx} -> val * idx end)
    |> Enum.sum()
  end
end
