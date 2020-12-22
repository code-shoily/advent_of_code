defmodule AdventOfCode.Y2020.Day22 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/22
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 22

  def run_1, do: input!() |> process() |> play() |> score()
  def run_2, do: input!() |> process() |> recursive_combat() |> score()

  def process(input) do
    input
    |> String.split("\n\n")
    |> Enum.flat_map(&String.split(&1, ":"))
    |> decks()
  end

  defp score(cards) do
    cards
    |> Enum.reverse()
    |> Enum.with_index(1)
    |> Enum.map(fn {val, idx} -> val * idx end)
    |> Enum.sum()
  end

  defp decks([_, player_1, _, player_2]), do: {deck(player_1), deck(player_2)}
  defp deck(player), do: Enum.map(String.split(player, "\n", trim: true), &String.to_integer/1)

  defp play({[], player_2}), do: player_2
  defp play({player_1, []}), do: player_1

  defp play({[card_1 | rest_1], [card_2 | rest_2]}) when card_1 > card_2,
    do: play({rest_1 ++ [card_1, card_2], rest_2})

  defp play({[card_1 | rest_1], [card_2 | rest_2]}),
    do: play({rest_1, rest_2 ++ [card_2, card_1]})

  defp recursive_combat(decks), do: decks |> recursive_combat([], []) |> elem(1)
  defp recursive_combat({[], player_2}, _, _), do: {2, player_2}
  defp recursive_combat({player_1, []}, _, _), do: {1, player_1}

  defp recursive_combat({[card_1 | rest_1] = a, [card_2 | rest_2] = b}, h1, h2) do
    if a in h1 || b in h2 do
      {1, a}
    else
      h1 = [a | h1]
      h2 = [b | h2]

      if card_1 <= length(rest_1) && card_2 <= length(rest_2) do
        case recursive_combat({Enum.take(rest_1, card_1), Enum.take(rest_2, card_2)}, [], []) do
          {1, _} -> recursive_combat({rest_1 ++ [card_1, card_2], rest_2}, h1, h2)
          {2, _} -> recursive_combat({rest_1, rest_2 ++ [card_2, card_1]}, h1, h2)
        end
      else
        case card_1 > card_2 do
          true -> recursive_combat({rest_1 ++ [card_1, card_2], rest_2}, h1, h2)
          _ -> recursive_combat({rest_1, rest_2 ++ [card_2, card_1]}, h1, h2)
        end
      end
    end
  end
end
