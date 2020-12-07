defmodule AdventOfCode.Y2020.Day7 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/7
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 7

  @mybag "shiny gold"

  def run_1, do: process() |> build_graph() |> sources()
  def run_2, do: bags(process(), @mybag) - 1
  def run, do: {run_1(), run_2()}

  def process(input \\ input!()) do
    input
    |> String.split("\n")
    |> Enum.map(&parse/1)
    |> Enum.into(%{})
  end

  defp parse(sentence) do
    [container, contained] = String.split(sentence, " bags contain ")
    {container, split_bags(contained)}
  end

  defp split_bags(bags) do
    bags
    |> String.split(",")
    |> Enum.map(fn bag ->
      bag
      |> String.replace(".", "")
      |> String.replace(~r/ bags?/, "")
      |> String.trim_leading()
      |> String.split(" ")
      |> to_bag_count()
    end)
  end

  defp to_bag_count([number | bag]) do
    number = (number == "no" && "0") || number
    {String.to_integer(number), Enum.join(bag, " ")}
  end

  defp build_graph(vertices) do
    Enum.reduce(vertices, :digraph.new(), fn v, graph -> build_edges(graph, v) end)
  end

  defp build_edges(graph, {node, edges}) do
    :digraph.add_vertex(graph, node)

    Enum.reduce(edges, graph, fn {_, dst}, graph ->
      :digraph.add_vertex(graph, dst)
      :digraph.add_edge(graph, node, dst)
      graph
    end)
  end

  defp sources(graph) do
    graph
    |> :digraph.vertices()
    |> Enum.filter(&:digraph.get_path(graph, &1, @mybag))
    |> Enum.count()
  end

  def bags(map, key) do
    Enum.reduce(Map.get(map, key, []), 1, fn {n, k}, acc ->
      acc + n * bags(map, k)
    end)
  end
end
