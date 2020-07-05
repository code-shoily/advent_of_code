defmodule AdventOfCode.Y2016.Day7 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2016/day/7
  """

  use AdventOfCode.Data.InputReader, year: 2016, day: 7

  def process(input) do
    String.split(input, "\n", trim: true) |> Enum.map(&parse_line/1)
  end

  def run_1 do
    input!()
    |> process()
    |> Enum.filter(&supports_tls?/1)
    |> Enum.count()
  end

  @pattern ~r/\[(.*?)\]/
  defp parse_line(line) do
    %{
      hypernets: @pattern |> Regex.scan(line) |> Enum.map(&Enum.at(&1, 1)),
      sequences: String.split(line, @pattern)
    }
  end

  defp abba?([a, b, b, a]), do: a != b
  defp abba?(_), do: false

  def contains_abba?(sequences) when is_list(sequences) do
    sequences
    |> Enum.reduce_while(false, fn sequence, _ ->
      (contains_abba?(sequence) && {:halt, true}) || {:cont, false}
    end)
  end

  def contains_abba?(sequence) do
    sequence
    |> String.codepoints()
    |> Enum.chunk_every(4, 1, :discard)
    |> Enum.reduce_while(false, fn subseq, _ ->
      (abba?(subseq) && {:halt, true}) || {:cont, false}
    end)
  end

  defp supports_tls?(%{hypernets: hypernets, sequences: sequences}) do
    not contains_abba?(hypernets) and contains_abba?(sequences)
  end

  def run_2 do
    input!()
    |> process()
    |> Enum.filter(&supports_ssl?/1)
    |> Enum.count()
  end

  defp supports_ssl?(%{hypernets: hypernets, sequences: sequences}) do
    with abas <- Enum.flat_map(sequences, &get_abas/1),
         true <- not Enum.empty?(abas),
         valid_babs <- Enum.map(abas, &bab_for/1),
         babs <- Enum.flat_map(hypernets, &get_abas/1),
         overlaps <- MapSet.intersection(MapSet.new(valid_babs), MapSet.new(babs)) do
      MapSet.size(overlaps) > 0
    end
  end

  defp get_abas(sequence) do
    sequence
    |> String.codepoints()
    |> Enum.chunk_every(3, 1, :discard)
    |> Enum.filter(&aba?/1)
  end

  defp aba?([a, b, a]), do: a != b
  defp aba?(_), do: false

  defp bab_for([a, b, a]), do: [b, a, b]
  defp bab_for(_), do: :error

  def run, do: {run_1(), run_2()}
end
