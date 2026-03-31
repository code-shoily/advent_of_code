defmodule AdventOfCode.Y2025.Day04 do
  @moduledoc """
  --- Day 4: Printing Department ---
  Problem Link: https://adventofcode.com/2025/day/4
  Difficulty: m
  Tags: simulation grid graph
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.{Builder.Grid, Model, Transform}

  def input, do: InputReader.read_from_file(2025, 4)

  def run(input \\ input()) do
    graph = parse(input)

    {run_1(graph), run_2(graph)}
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(&String.graphemes/1)
    |> Grid.from_2d_list_with_topology(:undirected, Grid.queen(), Grid.always())
    |> Grid.to_graph()
    |> Transform.filter_nodes(fn char -> char == "@" end)
  end

  @spec run_1(Yog.Graph.t()) :: non_neg_integer()
  def run_1(graph) do
    graph
    |> Enum.count(fn {id, _} -> length(Model.neighbor_ids(graph, id)) < 4 end)
  end

  def run_2(graph) do
    initial_removable =
      graph
      |> Enum.filter(fn {id, _} -> length(Model.neighbor_ids(graph, id)) < 4 end)
      |> Enum.map(fn {id, _} -> id end)

    process_removals(graph, initial_removable, 0)
  end

  defp process_removals(graph, removable, count) do
    case removable do
      [] ->
        count

      [curr | rest] ->
        if Model.has_node?(graph, curr) do
          neighbors = Model.neighbor_ids(graph, curr)
          new_graph = Model.remove_node(graph, curr)

          newly_removable =
            Enum.filter(neighbors, fn nid ->
              Model.has_node?(new_graph, nid) and length(Model.neighbor_ids(new_graph, nid)) < 4
            end)

          process_removals(new_graph, newly_removable ++ rest, count + 1)
        else
          process_removals(graph, rest, count)
        end
    end
  end
end
