defmodule AdventOfCode.Y2025.Day03 do
  @moduledoc """
  --- Day 3: Lobby ---
  Problem Link: https://adventofcode.com/2025/day/3
  Difficulty: m
  Tags: greedy optimization stack
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2025, 3)

  def run(input \\ input()) do
    banks = parse(input)

    {solve(banks, 2), solve(banks, 12)}
  end

  defp solve(banks, k) do
    banks
    |> Enum.map(fn digits -> max_subsequence(digits, k) end)
    |> Enum.sum()
  end

  defp max_subsequence(digits, k) do
    total_len = length(digits)
    can_drop = total_len - k

    {stack, _can_drop} =
      Enum.reduce(digits, {[], can_drop}, fn d, {stk, drop} ->
        {new_stk, new_drop} = pop_greedy(stk, d, drop)
        {[d | new_stk], new_drop}
      end)

    stack
    |> Enum.reverse()
    |> Enum.take(k)
    |> Enum.join()
    |> String.to_integer()
  end

  defp pop_greedy([top | rest], d, drop) when drop > 0 and d > top do
    pop_greedy(rest, d, drop - 1)
  end

  defp pop_greedy(stk, _, drop), do: {stk, drop}

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      line
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
    end)
  end
end
