defmodule AdventOfCode.Y2016.Day7 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2016/day/7
  """

  use AdventOfCode.Data.InputReader, year: 2016, day: 7

  def process(input) do
    String.split(input, "\n", trim: true)
  end

  @pattern ~r/\[(.*?)\]/
  defp parse_line(line) do
    hypernets = @pattern |> Regex.scan(line) |> Enum.map(&Enum.at(&1, 1))
    sequences = String.split(line, @pattern)

    {hypernets, sequences}
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

  defp supports_tls?({hypernets, sequences}) do
    not contains_abba?(hypernets) and contains_abba?(sequences)
  end

  def run_1 do
    input!()
    |> process()
    |> Enum.map(&parse_line/1)
    |> Enum.filter(&supports_tls?/1)
    |> Enum.count()
  end

  def run_2 do
    {:not_implemented, 2}
  end

  def run, do: {run_1(), run_2()}
end
