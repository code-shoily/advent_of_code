defmodule AdventOfCode.Y2023.Day04 do
  @moduledoc """
  --- Day 4: Scratchcards ---
  Problem Link: https://adventofcode.com/2023/day/4
  Difficulty: xs
  Tags: set reduction
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 4)

  def run(input \\ input()) do
    input = parse(input)
    card_map = input |> Enum.map(fn {id, _} -> id end) |> Map.new(fn k -> {k, 1} end)

    {run_1(input), run_2(input, card_map)}
  end

  defp run_1(input) do
    Enum.reduce(input, 0, fn
      {_, one_or_none}, acc when one_or_none < 2 -> acc + one_or_none
      {_, matches}, acc -> acc + 2 ** (matches - 1)
    end)
  end

  defp run_2(input, card_map) do
    with {end_of_table, _} <- Enum.max_by(input, fn {a, _} -> a end) do
      input
      |> Enum.reduce(card_map, &updated_cards(&1, end_of_table, &2))
      |> Map.values()
      |> Enum.sum()
    end
  end

  def parse(data \\ input()) do
    for line <- Transformers.lines(data), do: parse_card(line)
  end

  defp parse_card(line) do
    with ["Card", id | cards] <- String.split(line, ~r{\s+}),
         {winning, ["|" | got]} <- Enum.split_while(cards, fn v -> v != "|" end),
         {id, ":"} <- Integer.parse(id) do
      {id,
       MapSet.new(winning)
       |> MapSet.intersection(MapSet.new(got))
       |> MapSet.size()}
    end
  end

  def updated_cards({_, 0}, _, card_map), do: card_map

  def updated_cards({id, count}, end_of_table, card_map) do
    Stream.unfold(id + 1, fn n -> {n, n + 1} end)
    |> Enum.take(count)
    |> Enum.reject(&(&1 > end_of_table))
    |> Map.new(fn k -> {k, Map.fetch!(card_map, id)} end)
    |> Map.merge(card_map, fn _, v1, v2 -> v1 + v2 end)
  end
end
