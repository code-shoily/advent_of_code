defmodule AdventOfCode.Y2022.Day16 do
  @moduledoc """
  --- Day 16: Proboscidea Volcanium ---
  Problem Link: https://adventofcode.com/2022/day/16
  Difficulty: hard
  Tags: graph floyd-warshall optimization
  """
  import Bitwise
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Pathfinding.FloydWarshall

  def input, do: InputReader.read_from_file(2022, 16)

  def run(input \\ input()) do
    valves = parse(input)
    dist_map = build_dist_map(valves)

    parts_map = explore(valves, dist_map, 30)
    p1 = parts_map |> Map.values() |> Enum.max()

    p2_map = explore(valves, dist_map, 26)

    p2 =
      for {m1, f1} <- p2_map, {m2, f2} <- p2_map, band(m1, m2) == 0, reduce: 0 do
        acc -> max(acc, f1 + f2)
      end

    {p1, p2}
  end

  defp explore(valves, dist_map, time) do
    # Only care about valves with flow > 0
    relevant =
      valves |> Enum.filter(fn {_, f, _} -> f > 0 end) |> Enum.map(fn {id, f, _} -> {id, f} end)

    indices = relevant |> Enum.with_index() |> Map.new(fn {{id, _}, i} -> {id, i} end)

    memo = dfs("AA", time, 0, 0, relevant, dist_map, indices, %{})
    memo
  end

  defp dfs(curr, time, mask, flow, relevant, dist_map, indices, acc) do
    acc = Map.update(acc, mask, flow, &max(&1, flow))

    Enum.reduce(relevant, acc, fn {next, f}, acc ->
      idx = indices[next]
      dist = dist_map[{curr, next}]
      rem_time = time - dist - 1

      if band(mask, bsl(1, idx)) == 0 and rem_time > 0 do
        dfs(
          next,
          rem_time,
          bor(mask, bsl(1, idx)),
          flow + rem_time * f,
          relevant,
          dist_map,
          indices,
          acc
        )
      else
        acc
      end
    end)
  end

  defp build_dist_map(valves) do
    graph = Yog.directed()

    graph =
      Enum.reduce(valves, graph, fn {id, _, neighbors}, g ->
        Enum.reduce(neighbors, g, fn n, g_acc ->
          Yog.add_edge_ensure(g_acc, id, n, 1)
        end)
      end)

    # Use Floyd-Warshall to get all-pairs shortest paths
    {:ok, res} = FloydWarshall.floyd_warshall(graph)

    # relevant = AA + all with flow > 0
    ids = [
      "AA" | valves |> Enum.filter(fn {_, f, _} -> f > 0 end) |> Enum.map(fn {id, _, _} -> id end)
    ]

    for u <- ids, v <- ids, into: %{} do
      case Map.fetch(res, {u, v}) do
        {:ok, d} -> {{u, v}, d}
        # unreachable
        :error -> {{u, v}, 1000}
      end
    end
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [_, id, flow, tunnels] =
        Regex.run(~r/Valve ([A-Z]{2}) has flow rate=(\d+); tunnels? leads? to valves? (.*)/, line)

      {id, String.to_integer(flow), String.split(tunnels, ", ")}
    end)
  end
end
