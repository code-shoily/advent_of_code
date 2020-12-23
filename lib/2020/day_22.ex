defmodule AdventOfCode.Y2020.Day22 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/22
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 22

  def run_1, do: input!() |> process() |> play() |> score()
  def run_2, do: input!() |> process() |> play_rec() |> score()
  def process(deal), do: decks(Enum.map(String.split(deal, "\n\n"), &String.split(&1, ":")))

  defp score(cards) do
    Enum.reduce(Enum.with_index(Enum.reverse(cards), 1), 0, fn {val, idx}, score ->
      score + val * idx
    end)
  end

  defp decks([[_, human], [_, crab]]), do: {deck(human), deck(crab)}
  defp deck(p), do: Enum.map(String.split(p, "\n", trim: true), &String.to_integer/1)

  defp play({[], crab}), do: crab
  defp play({human, []}), do: human
  defp play({[h | human], [c | crab]}) when h > c, do: play({human ++ [h, c], crab})
  defp play({[h | human], [c | crab]}), do: play({human, crab ++ [c, h]})

  defp play_rec(decks), do: elem(play_rec(decks, {[], []}), 1)
  defp play_rec({[], crab}, _), do: {:crab, crab}
  defp play_rec({human, []}, _), do: {:human, human}

  defp play_rec({[h | hs] = human, [c | cs] = crab}, {humans, crabs}) do
    if human in humans || crab in crabs do
      {:human, human}
    else
      memo = {[human | humans], [crab | crabs]}

      if h <= length(hs) && c <= length(cs) do
        case play_rec({Enum.take(hs, h), Enum.take(cs, c)}, {[], []}) do
          {:human, _} -> play_rec({hs ++ [h, c], cs}, memo)
          {:crab, _} -> play_rec({hs, cs ++ [c, h]}, memo)
        end
      else
        (h > c && play_rec({hs ++ [h, c], cs}, memo)) || play_rec({hs, cs ++ [c, h]}, memo)
      end
    end
  end
end
