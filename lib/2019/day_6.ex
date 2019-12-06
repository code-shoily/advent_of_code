defmodule AdventOfCode.Y2019.Day6 do
  @moduledoc """
  Problem Description: https://adventofcode.com/2019/day/6
  """
  use AdventOfCode.Data.InputReader, year: 2019, day: 6

  def process() do
    input!()
    |> String.split("\n")
    |> Enum.map(&String.split(&1, ")"))
  end

  def to_graph(data, graph) do
    data
    |> Enum.each(fn [a, b] ->
      :digraph.add_vertex(graph, a)
      :digraph.add_vertex(graph, b)
      :digraph.add_edge(graph, b, a)
    end)

    graph
  end

  def count_orbits(graph) do
    graph
    |> :digraph.vertices()
    |> Stream.map(fn vertex ->
      :digraph.get_path(graph, vertex, "COM") || []
    end)
    |> Stream.map(fn path -> Enum.count(path) - 1 end)
    |> Enum.sum()
    |> Kernel.+(1)
  end

  def run_1 do
    process()
    |> to_graph(:digraph.new([:acyclic, :private]))
    |> count_orbits()
  end

  def run_2 do
    0
  end

  def run do
    %{problem_1: run_1(), problem_2: run_2()}
  end
end
