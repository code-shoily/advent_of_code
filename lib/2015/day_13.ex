defmodule AdventOfCode.Y2015.Day13 do
  @moduledoc """
  --- Day 13: Knights of the Dinner Table ---
  Problem Link: https://adventofcode.com/2015/day/13
  Difficulty: s
  Tags: graph dynamic-programming
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Model
  import Bitwise

  def input, do: InputReader.read_from_file(2015, 13)

  def run(input \\ input()) do
    graph = parse(input)

    {
      solve(graph),
      solve(add_me(graph))
    }
  end

  def parse(data \\ input()) do
    parsed =
      data
      |> Transformers.lines()
      |> Enum.map(&parse_fact/1)

    graph =
      Enum.reduce(parsed, Yog.undirected(), fn {a, b, _w}, g ->
        g |> Model.add_node(a, nil) |> Model.add_node(b, nil)
      end)

    Enum.reduce(parsed, graph, fn {a, b, w}, g ->
      Model.add_edge_with_combine!(g, a, b, w, &Kernel.+/2)
    end)
  end

  @regex ~r/(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\./
  def parse_fact(statement) do
    @regex
    |> Regex.run(statement, capture: :all_but_first)
    |> then(fn [person_a, action, value, person_b] ->
      {
        person_a,
        person_b,
        String.to_integer(value) * if(action == "gain", do: 1, else: -1)
      }
    end)
  end

  defp add_me(graph) do
    people = Model.all_nodes(graph)

    Enum.reduce(people, graph, fn person, g ->
      Model.add_edge_ensure(g, "Me", person, 0)
    end)
  end

  defp solve(graph) do
    nodes = Model.all_nodes(graph)
    [root | _rest] = nodes
    nodes_indexed = Enum.with_index(nodes)
    node_to_idx = Map.new(nodes_indexed)
    _idx_to_node = Map.new(Enum.map(nodes_indexed, fn {n, i} -> {i, n} end))

    weights =
      for {u, i} <- nodes_indexed, {v, j} <- nodes_indexed, into: %{} do
        {{i, j}, edge_weight(graph, u, v)}
      end

    root_idx = node_to_idx[root]
    num_nodes = length(nodes)
    target_mask = (1 <<< num_nodes) - 1

    Process.put(:tsp_memo, %{})
    tsp(root_idx, 1 <<< root_idx, root_idx, target_mask, weights, num_nodes)
  end

  defp tsp(curr_idx, mask, root_idx, target_mask, weights, n) do
    if mask == target_mask do
      weights[{curr_idx, root_idx}]
    else
      memo = Process.get(:tsp_memo)

      if Map.has_key?(memo, {curr_idx, mask}) do
        memo[{curr_idx, mask}]
      else
        res =
          for next_idx <- 0..(n - 1), (mask &&& 1 <<< next_idx) == 0 do
            weights[{curr_idx, next_idx}] +
              tsp(next_idx, bor(mask, 1 <<< next_idx), root_idx, target_mask, weights, n)
          end
          |> Enum.max()

        Process.put(:tsp_memo, Map.put(memo, {curr_idx, mask}, res))
        res
      end
    end
  end

  defp edge_weight(graph, u, v) do
    case Enum.find(Model.successors(graph, u), fn {id, _} -> id == v end) do
      {_, w} -> w
      nil -> 0
    end
  end
end
