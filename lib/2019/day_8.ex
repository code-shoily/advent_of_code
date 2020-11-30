defmodule AdventOfCode.Y2019.Day8 do
  @moduledoc """
  Problem description: https://adventofcode.com/2019/day/8
  """
  use AdventOfCode.Helpers.InputReader, year: 2019, day: 8

  @width 25
  @height 6
  @printchar "▒"

  def run_1 do
    process()
    |> Enum.map(fn layer -> Enum.flat_map(layer, & &1) end)
    |> Enum.min_by(&Enum.count(&1, fn x -> x == 0 end))
    |> (fn x -> Enum.count(x, &(&1 == 2)) * Enum.count(x, &(&1 == 1)) end).()
  end

  def run_2 do
    process()
    |> Enum.map(fn layer -> Enum.flat_map(layer, & &1) end)
    |> Enum.reduce(&overlay(&2, &1))
    |> Enum.join()
    |> print_image()
  end

  def run, do: {run_1(), run_2()}

  def process do
    input!()
    |> String.codepoints()
    |> Enum.map(&String.to_integer/1)
    |> chunkify()
  end

  defp overlay(l1, l2) do
    l1
    |> Enum.zip(l2)
    |> Enum.map(fn
      {2, x} -> x
      {1, _} -> 1
      {0, _} -> 0
    end)
  end

  defp print_image(data) do
    data
    |> String.replace("1", @printchar)
    |> String.replace("0", " ")
    |> String.codepoints()
    |> chunkify()
    |> hd()
    |> Enum.map_join("\n", &Enum.join/1)
    |> IO.puts()
  end

  defp chunkify(data) do
    data
    |> Enum.chunk_every(@width * @height)
    |> Enum.map(&Enum.chunk_every(&1, @width))
  end
end
