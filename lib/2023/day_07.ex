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

  defp run_1(input) do
    input
    |> Enum.group_by(&elem(&1, 0))
    |> Enum.sort_by(fn {rank, _} -> rank end)
    |> Enum.flat_map(fn {_, hands} ->
      Enum.sort(hands, fn {_, h1, _}, {_, h2, _} -> smaller?(h1, h2, @card_rank_1) end)
    end)
    |> Enum.with_index(1)
    |> Enum.reduce(0, fn {{_, _, bid}, rank}, acc ->
      acc + bid * rank
    end)
  end

  defp run_2(input) do
    input
    |> Enum.map(fn {rank, hand, bid} ->
      {get_updated_rank(rank, hand), hand, bid}
    end)
    |> Enum.group_by(&elem(&1, 0))
    |> Enum.sort_by(fn {rank, _} -> rank end)
    |> Enum.flat_map(fn {_, hands} ->
      Enum.sort(hands, fn {_, h1, _}, {_, h2, _} -> smaller?(h1, h2, @card_rank_2) end)
    end)
    |> Enum.with_index(1)
    |> Enum.reduce(0, fn {{_, _, bid}, rank}, acc ->
      acc + bid * rank
    end)
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

  def get_updated_rank(rank, hand) do
    freq = frequency(hand)

    case {rank, freq["J"]} do
      # No J present
      {_, nil} -> rank
      # high card
      {1, _} -> 2
      # 1 pair
      {2, _} -> 4
      # 2 pairs
      {3, 1} -> 5
      {3, 2} -> 6
      # 3 of a kind
      {4, _} -> 6
      # full house
      {5, _} -> 7
      # 4 of a kind
      {6, _} -> 7
      # no change - highest rank
      {7, _} -> rank
    end
  end

  def get_rank(hand) do
    values = hand |> frequency() |> Map.values() |> Enum.sort()

    cond do
      # High Card
      [1, 1, 1, 1, 1] == values -> 1
      # 1 Pair
      [1, 1, 1, 2] == values -> 2
      # 2 Pairs
      [1, 2, 2] == values -> 3
      # 3 of a kind
      [1, 1, 3] == values -> 4
      # Full House
      [2, 3] == values -> 5
      # 4 of a kind
      [1, 4] == values -> 6
      # 5 of a kind
      [5] == values -> 7
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
