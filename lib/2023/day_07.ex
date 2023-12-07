defmodule AdventOfCode.Y2023.Day07 do
  @moduledoc """
  --- Day 7: Camel Cards ---
  Problem Link: https://adventofcode.com/2023/day/7
  Difficulty: m
  Tags: map pattern-matching
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @card_rank_1 %{
    "2" => 1,
    "3" => 2,
    "4" => 3,
    "5" => 4,
    "6" => 5,
    "7" => 6,
    "8" => 7,
    "9" => 8,
    "T" => 9,
    "J" => 10,
    "Q" => 11,
    "K" => 12,
    "A" => 13
  }

  @card_rank_2 %{@card_rank_1 | "J" => 0}

  def input, do: InputReader.read_from_file(2023, 7)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input), do: get_winnings(input, @card_rank_1)

  defp run_2(input) do
    input
    |> Enum.map(fn {rank, hand, bid} -> {jokered_rank(rank, hand), hand, bid} end)
    |> get_winnings(@card_rank_2)
  end

  def get_winnings(card_configuration, value_mapping) do
    card_configuration
    |> Enum.group_by(&elem(&1, 0))
    |> Enum.sort_by(fn {rank, _} -> rank end)
    |> Enum.flat_map(fn {_, hands} ->
      hands |> Enum.sort(fn {_, h1, _}, {_, h2, _} -> smaller?(h1, h2, value_mapping) end)
    end)
    |> Enum.with_index(1)
    |> Enum.reduce(0, fn {{_, _, bid}, rank}, acc -> acc + bid * rank end)
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn card ->
      [hand, bid] = String.split(card, " ")
      hand = String.graphemes(hand)
      {get_rank(hand), hand, String.to_integer(bid)}
    end)
  end

  def frequency(hand) do
    hand
    |> Enum.group_by(& &1)
    |> Map.new(fn {a, b} -> {a, length(b)} end)
  end

  def jokered_rank(rank, hand) do
    freq = frequency(hand)

    case {rank, freq["J"]} do
      {_, nil} -> rank
      {1, _} -> 2
      {2, _} -> 4
      {3, 1} -> 5
      {3, 2} -> 6
      {4, _} -> 6
      {5, _} -> 7
      {6, _} -> 7
      {7, _} -> rank
    end
  end

  def get_rank(hand) do
    case hand |> frequency() |> Map.values() |> Enum.sort() do
      [1, 1, 1, 1, 1] -> 1
      [1, 1, 1, 2] -> 2
      [1, 2, 2] -> 3
      [1, 1, 3] -> 4
      [2, 3] -> 5
      [1, 4] -> 6
      [5] -> 7
    end
  end

  def smaller?([a | rest_1], [b | rest_2], mapping) do
    cond do
      mapping[a] > mapping[b] -> false
      mapping[a] < mapping[b] -> true
      true -> smaller?(rest_1, rest_2, mapping)
    end
  end
end
