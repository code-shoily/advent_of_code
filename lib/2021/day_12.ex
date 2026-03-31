defmodule AdventOfCode.Y2021.Day12 do
  @moduledoc """
  --- Day 12: Passage Pathing ---
  Problem Link: https://adventofcode.com/2021/day/12
  Difficulty: m
  Tags: graph traversal recursion path-finding
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2021, 12)

  def run(input \\ input()) do
    graph = parse(input)

    p1 = count_paths(graph, "start", MapSet.new(), false)
    p2 = count_paths(graph, "start", MapSet.new(), true)

    {p1, p2}
  end

  defp count_paths(_graph, "end", _visited, _allow_double), do: 1

  defp count_paths(graph, current, visited, allow_double) do
    next_visited = if small?(current), do: MapSet.put(visited, current), else: visited

    Yog.neighbors(graph, current)
    |> Enum.reduce(0, fn {neighbor, _}, total ->
      cond do
        neighbor == "start" ->
          total

        not small?(neighbor) or not MapSet.member?(visited, neighbor) ->
          total + count_paths(graph, neighbor, next_visited, allow_double)

        allow_double ->
          total + count_paths(graph, neighbor, next_visited, false)

        true ->
          total
      end
    end)
  end

  defp small?(node), do: String.downcase(node) == node

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.reduce(Yog.undirected(), fn line, graph ->
      [u, v] = String.split(line, "-")
      Yog.add_edge_ensure(graph, u, v, 1)
    end)
  end
end
