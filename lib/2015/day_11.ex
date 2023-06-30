defmodule AdventOfCode.Y2015.Day11 do
  @moduledoc """
  --- Day 11: Corporate Policy ---
  Problem Link: https://adventofcode.com/2015/day/11
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2015, 11)

  def run(input \\ input()) do
    part_1 = next_valid_password(input)
    {part_1, next_valid_password(part_1)}
  end

  def next_valid_password(password) do
    next_sequence = next_sequence(password)

    if valid?(next_sequence) do
      next_sequence
    else
      next_valid_password(next_sequence)
    end
  end

  def next_sequence(password),
    do: next_sequence([], password |> String.graphemes() |> Enum.reverse())

  def next_sequence(prefix, []), do: Enum.join(prefix)

  def next_sequence(prefix, [current]), do: next_sequence([next_char(current) | prefix], [])

  def next_sequence(prefix, [current, upcoming | suffix]) do
    new_current = next_char(current)
    prefix = [new_current | prefix]

    if new_current == "a" do
      next_sequence(prefix, [upcoming | suffix])
    else
      String.reverse(Enum.join(Enum.reverse(prefix)) <> Enum.join([upcoming | suffix]))
    end
  end

  def next_char(current) do
    case String.to_charlist(current) do
      ~c"h" -> ~c"j"
      ~c"k" -> ~c"m"
      ~c"n" -> ~c"p"
      ~c"z" -> ~c"a"
      c -> hd(c) + 1
    end
    |> List.wrap()
    |> to_string()
  end

  def valid?(password) do
    does_not_contain_invalid_chars(password) && has_sequencial_chars(password) &&
      contains_pair(password)
  end

  defp has_sequencial_chars(password) do
    password
    |> String.to_charlist()
    |> Enum.chunk_every(3, 1, :discard)
    |> Enum.any?(fn [a, b, c] -> c - b == 1 and b - a == 1 end)
  end

  defp does_not_contain_invalid_chars(password) do
    password
    |> String.graphemes()
    |> Enum.all?(fn c -> c not in ["i", "o", "l"] end)
  end

  defp contains_pair(password) do
    password
    |> String.graphemes()
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.filter(fn [a, b] -> a == b end)
    |> Enum.uniq()
    |> then(fn pairs -> length(pairs) > 1 end)
  end
end
