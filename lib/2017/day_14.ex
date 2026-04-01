defmodule AdventOfCode.Y2017.Day14 do
  @moduledoc """
  --- Day 14: Disk Defragmentation ---
  Problem Link: https://adventofcode.com/2017/day/14
  Difficulty: m
  Tags: graph hash
  """
  alias AdventOfCode.Helpers.InputReader
  alias AdventOfCode.Y2017.Day10

  def input, do: InputReader.read_from_file(2017, 14)

  alias Yog.Connectivity

  def run(input \\ input()) do
    on_set = input |> parse() |> build_grid()

    {run_1(on_set), run_2(on_set)}
  end

  def parse(data \\ input()), do: String.trim(data)

  defp run_1(on_set), do: MapSet.size(on_set)

  defp run_2(on_set) do
    on_set
    |> Enum.reduce(Yog.undirected(), fn {r, c} = pos, graph ->
      neighbors =
        [{r + 1, c}, {r, c + 1}]
        |> Enum.filter(&MapSet.member?(on_set, &1))

      Enum.reduce(neighbors, Yog.add_node(graph, pos, nil), fn neighbor, acc ->
        Yog.add_edge_ensure(acc, pos, neighbor, 1)
      end)
    end)
    |> Connectivity.connected_components()
    |> length()
  end

  defp build_grid(input) do
    0..127
    |> Task.async_stream(fn r ->
      "#{input}-#{r}"
      |> Day10.compute_knot_hash()
      |> to_bits()
      |> Enum.with_index()
      |> Enum.filter(fn {bit, _} -> bit == "1" end)
      |> Enum.map(fn {_, c} -> {r, c} end)
    end)
    |> Enum.reduce(MapSet.new(), fn {:ok, row_points}, acc ->
      Enum.reduce(row_points, acc, &MapSet.put(&2, &1))
    end)
  end

  defp to_bits(hash) do
    hash
    |> String.graphemes()
    |> Enum.flat_map(fn hex ->
      hex
      |> String.to_integer(16)
      |> Integer.to_string(2)
      |> String.pad_leading(4, "0")
      |> String.graphemes()
    end)
  end
end
