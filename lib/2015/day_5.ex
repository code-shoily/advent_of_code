defmodule AdventOfCode.Y2015.Day5 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/5
  """
  use AdventOfCode.Helpers.InputReader, year: 2015, day: 5

  def run_1 do
    process()
    |> Enum.filter(&nice?/1)
    |> length()
  end

  def run_2 do
    process()
    |> Enum.filter(&nice_v2?/1)
    |> length()
  end

  def run, do: {run_1(), run_2()}

  defp process, do: String.split(input!(), "\n")

  defp vowel?(s), do: s in ~w/a e i o u/

  defp nice?(s), do: nice?(s, 0, 0, "")
  defp nice?("", vowels, dups, _) when vowels >= 3 and dups > 0, do: true
  defp nice?("", _, _, _), do: false

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
