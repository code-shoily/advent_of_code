defmodule AdventOfCode.Y2020.Day22 do
  @moduledoc """
  --- Day 22: Crab Combat ---
  Problem Link: https://adventofcode.com/2020/day/22
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 22)

  def run(input \\ input()) do
    input = parse(input)
    {score(play(input)), score(play_rec(input))}
  end

  def parse(deal),
    do: decks(Enum.map(String.split(deal, ~r{(\r\n\r\n|\r\r|\n\n)}), &String.split(&1, ":")))

  defp score(cards) do
    Enum.reduce(Enum.with_index(Enum.reverse(cards), 1), 0, fn {val, idx}, score ->
      score + val * idx
    end)
  end

  defp decks([[_, human], [_, crab]]), do: {deck(human), deck(crab)}
  defp deck(p), do: Enum.map(Transformers.lines(p), &String.to_integer/1)

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
        # credo:disable-for-next-line
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
