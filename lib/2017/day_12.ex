defmodule AdventOfCode.Y2017.Day12 do
  @moduledoc """
  --- Day 12: Digital Plumber ---
  Problem Link: https://adventofcode.com/2017/day/12
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 12

  alias AdventOfCode.Helpers.Transformers
  alias ExAlgo.Set.DisjointSet

  def run(input \\ input!()) do
    disjoint_set = input |> parse() |> create_disjoint_set()
    nodes = Map.keys(disjoint_set.parents)

    {run_1(disjoint_set, nodes), run_2(disjoint_set, nodes)}
  end

  defp run_1(disjoint_set, nodes) do
    disjoint_set
    |> find_connections_for(nodes, 0)
    |> length()
  end

  defp run_2(disjoint_set, nodes) do
    disjoint_set
    |> find_groups(nodes)
    |> Enum.count()
  end

  defp find_connections_for(disjoint_set, nodes, node) do
    {target_parent, disjoint_set} = DisjointSet.find(disjoint_set, node)

    nodes
    |> Enum.reduce({[], disjoint_set}, fn x, {lst, set} ->
      case DisjointSet.find(set, x) do
        {^target_parent, new_set} -> {[x | lst], new_set}
        {_, new_set} -> {lst, new_set}
      end
    end)
    |> elem(0)
  end

  defp create_disjoint_set(input) do
    disjoint_set = DisjointSet.new(Enum.count(input))

    input
    |> Enum.reduce(disjoint_set, fn {node, conns}, acc ->
      Enum.reduce(conns, acc, fn y, s ->
        DisjointSet.union(s, node, y)
      end)
    end)
  end

  defp find_groups(disj_set, nodes) do
    nodes
    |> Enum.reduce({MapSet.new(), disj_set}, fn x, {nodes, set} ->
      {parent, new_set} = DisjointSet.find(set, x)
      {MapSet.put(nodes, parent), new_set}
    end)
    |> elem(0)
  end

  def parse(data \\ input!()) do
    data
    |> Transformers.lines()
    |> Map.new(fn line ->
      [node, connections] = line |> String.split(" <-> ")
      {String.to_integer(node), Transformers.int_words(connections, ", ")}
    end)
  end
end
