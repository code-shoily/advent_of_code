defmodule AdventOfCode.Y2019.Day06 do
  @moduledoc """
  --- Day 6: Universal Orbit Map ---
  Problem Link: https://adventofcode.com/2019/day/6
  """
  use AdventOfCode.Helpers.InputReader, year: 2019, day: 6

  def run_1 do
    input!()
    |> parse()
    |> to_graph(:digraph.new([:acyclic]))
    |> count_orbits()
  end

  def run_2 do
    input!()
    |> parse()
    |> to_graph(:digraph.new())
    |> count_orbital_transfers()
  end

  def parse(data) do
    data |> String.split("\n") |> Enum.map(&String.split(&1, ")"))
  end

  defp to_graph(data, graph) do
    data
    |> Enum.each(fn [a, b] ->
      :digraph.add_vertex(graph, a)
      :digraph.add_vertex(graph, b)
      :digraph.add_edge(graph, b, a)
      :digraph.add_edge(graph, a, b)
    end)

    graph
  end

  defp count_orbits(graph) do
    graph
    |> :digraph.vertices()
    |> Stream.map(fn vertex ->
      :digraph.get_path(graph, vertex, "COM") || []
    end)
    |> Stream.map(fn path -> length(path) - 1 end)
    |> Enum.sum()
    |> Kernel.+(1)
  end

  defp count_orbital_transfers(graph) do
    graph
    |> :digraph.get_short_path("YOU", "SAN")
    |> Enum.count()
    |> Kernel.-(3)
  end
end
