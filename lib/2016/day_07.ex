defmodule AdventOfCode.Y2016.Day07 do
  @moduledoc """
  --- Day 7: Internet Protocol Version 7 ---
  Problem Link: https://adventofcode.com/2016/day/7
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2016, 7)

  def run(input \\ input()) do
    input = parse(input)

    {count_ipv7(input, &supports_tls?/1), count_ipv7(input, &supports_ssl?/1)}
  end

  def count_ipv7(input, pred), do: Enum.count(input, pred)

  def parse(input) do
    for line <- Transformers.lines(input), do: parse_line(line)
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
    Enum.reduce_while(sequences, false, fn sequence, _ ->
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
end
