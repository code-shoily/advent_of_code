defmodule AdventOfCode.Y2025.Day11 do
  @moduledoc """
  --- Day 11: Reactor ---
  Problem Link: https://adventofcode.com/2025/day/11
  Difficulty: m
  Tags: dynamic-programming graph
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2025, 11)

  def run(input \\ input()) do
    graph = parse(input)

    {run_1(graph), run_2(graph)}
  end

  def run_1(graph) do
    do_count_paths(graph, "you", "out")
  end

  def run_2(graph) do
    p1 =
      do_count_paths(graph, "svr", "dac") *
        do_count_paths(graph, "dac", "fft") *
        do_count_paths(graph, "fft", "out")

    p2 =
      do_count_paths(graph, "svr", "fft") *
        do_count_paths(graph, "fft", "dac") *
        do_count_paths(graph, "dac", "out")

    p1 + p2
  end

  defp do_count_paths(_graph, source, source), do: 1

  defp do_count_paths(graph, source, target) do
    if Yog.Traversal.reachable?(graph, source, target) do
      {:ok, order} = Yog.Traversal.topological_sort(graph)

      order
      |> Enum.reverse()
      |> Enum.reduce(%{}, fn node, acc ->
        if node == target do
          Map.put(acc, node, 1)
        else
          graph
          |> Yog.successor_ids(node)
          |> Enum.reduce(0, fn succ, sum ->
            sum + Map.get(acc, succ, 0)
          end)
          |> then(&Map.put(acc, node, &1))
        end
      end)
      |> Map.get(source, 0)
    else
      0
    end
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.reduce(Yog.directed(), fn line, g ->
      [src, dsts_str] = String.split(line, ":", parts: 2)

      dsts_str
      |> String.trim()
      |> String.split(" ", trim: true)
      |> Enum.reduce(g, fn dst, g_acc ->
        Yog.add_edge_ensure(g_acc, String.trim(src), dst, 1, nil)
      end)
    end)
  end
end
