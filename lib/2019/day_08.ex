defmodule AdventOfCode.Y2019.Day08 do
  @moduledoc """
  --- Day 8: Space Image Format ---
  Problem Link: https://adventofcode.com/2019/day/8
  Difficulty: xs
  Tags: sequence visual-result
  """
  alias AdventOfCode.Helpers.InputReader

  @width 25
  @height 6
  @printchar "▒"

  def input, do: InputReader.read_from_file(2019, 8)

  def run(input \\ input()) do
    input = parse(input)
    run_2(input)
    {run_1(input), :ok}
  end

  def run_1(input) do
    input
    |> Enum.map(fn layer -> Enum.flat_map(layer, & &1) end)
    |> Enum.min_by(&Enum.count(&1, fn x -> x == 0 end))
    |> then(fn x -> Enum.count(x, &(&1 == 2)) * Enum.count(x, &(&1 == 1)) end)
  end

  def run_2(input) do
    input
    |> Enum.map(fn layer -> Enum.flat_map(layer, & &1) end)
    |> Enum.reduce(&overlay(&2, &1))
    |> Enum.join()
    |> print_image()
  end

  def parse(data) do
    data
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> chunkify()
  end

  defp overlay(layer_1, layer_2) do
    layer_1
    |> Enum.zip(layer_2)
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
    |> List.first()
    |> Enum.map_join("\n", &Enum.join/1)
    |> tap(&IO.puts/1)
  end

  defp chunkify(data) do
    data
    |> Enum.chunk_every(@width * @height)
    |> Enum.map(&Enum.chunk_every(&1, @width))
  end
end
