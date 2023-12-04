defmodule AdventOfCode.Y2015.Day05 do
  @moduledoc """
  --- Day 5: Doesn't He Have Intern-Elves For This? ---
  Problem Link: https://adventofcode.com/2015/day/5
  Difficulty: xs
  Tags: string
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 5)

  def run(input \\ input()) do
    input = Transformers.lines(input)
    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    input
    |> Enum.filter(&nice?/1)
    |> length()
  end

  defp run_2(input) do
    input
    |> Enum.filter(&nice_v2?/1)
    |> length()
  end

  defp vowel?(s), do: s in ~w/a e i o u/

  defp nice?(s), do: nice?(s, 0, 0, "")
  defp nice?("", vowels, dups, _), do: vowels >= 3 and dups > 0

  defp nice?(<<s::bytes-size(1)>> <> rest, vowels, dups, e) do
    cond do
      (e <> s) in ~w/ab cd pq xy/ -> false
      vowel?(s) and s == e -> nice?(rest, vowels + 1, dups + 1, s)
      vowel?(s) -> nice?(rest, vowels + 1, dups, s)
      s == e -> nice?(rest, vowels, dups + 1, s)
      true -> nice?(rest, vowels, dups, s)
    end
  end

  defp nice_v2?(word), do: no_overlapping_pairs?(word) and sandwiched_letter?(word)

  defp no_overlapping_pairs?(word) do
    in_2s = word |> String.codepoints() |> Enum.chunk_every(2, 1, :discard) |> Enum.dedup()
    in_2s_unique = in_2s |> Enum.uniq()

    length(in_2s) != length(in_2s_unique) or four_or_more?(word)
  end

  defp four_or_more?(word) do
    word
    |> String.codepoints()
    |> Enum.chunk_by(& &1)
    |> Enum.map(&(length(&1) == 4))
    |> Enum.any?()
  end

  defp sandwiched_letter?(word) do
    word
    |> String.codepoints()
    |> Enum.chunk_every(3, 1, :discard)
    |> Enum.map(fn
      [a, _, a] -> true
      _ -> false
    end)
    |> Enum.any?()
  end
end
