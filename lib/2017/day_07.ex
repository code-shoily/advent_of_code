defmodule AdventOfCode.Y2017.Day07 do
  @moduledoc """
  --- Day 7: Recursive Circus ---
  Problem Link: https://adventofcode.com/2017/day/7
  Difficulty: m
  Tags: graph tree
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Model

  def input, do: InputReader.read_from_file(2017, 7)

  def run(input \\ input()) do
    graph = parse(input)

    {run_1(graph), run_2(graph)}
  end

  defp run_1(graph) do
    Yog.arborescence_root(graph)
  end

  defp run_2(graph) do
    root = run_1(graph)
    find_imbalance(graph, root)
  end

  defp find_imbalance(graph, node) do
    case find_unbalanced_child(graph, node) do
      {:imbalanced, result} ->
        result

      {:balanced, _weight} ->
        nil
    end
  end

  defp find_unbalanced_child(graph, node) do
    children = Model.successor_ids(graph, node)
    child_results = Enum.map(children, &{unit_and_total_weight(graph, &1), &1})

    imbalanced_case =
      child_results
      |> Enum.group_by(fn {{_, total}, _} -> total end)
      |> Enum.find(fn {_total, nodes} -> length(nodes) == 1 end)

    if imbalanced_case do
      {imbalanced_weight, [{_res, imbalanced_node}]} = imbalanced_case

      {balanced_weight, _} =
        Enum.find(child_results, fn {{_, t}, _} -> t != imbalanced_weight end) |> elem(0)

      case find_unbalanced_child(graph, imbalanced_node) do
        {:imbalanced, result} ->
          {:imbalanced, result}

        {:balanced, _} ->
          diff = balanced_weight - imbalanced_weight
          {:imbalanced, Model.node(graph, imbalanced_node) + diff}
      end
    else
      total =
        Model.node(graph, node) +
          (child_results |> Enum.map(fn {{_, t}, _} -> t end) |> Enum.sum())

      {:balanced, total}
    end
  end

  # Helper to get (own_weight, total_subtree_weight)
  defp unit_and_total_weight(graph, node) do
    own = Model.node(graph, node)

    total =
      Model.successor_ids(graph, node)
      |> Enum.map(&elem(unit_and_total_weight(graph, &1), 1))
      |> Enum.sum()
      |> Kernel.+(own)

    {own, total}
  end

  @regex ~r/(?<name>[a-z]+) \((?<weight>\d+)\)( -> (?<branches>(\s*[a-z]+\,?)+))?/
  def parse(data \\ input()) do
    parsed =
      data
      |> Transformers.lines()
      |> Enum.map(fn line -> Regex.named_captures(@regex, line) end)
      |> Enum.map(fn %{"branches" => branches, "weight" => weight, "name" => name} ->
        %{
          branches: (String.trim(branches) != "" && String.split(branches, ", ")) || [],
          name: name,
          weight: String.to_integer(weight)
        }
      end)

    graph =
      Enum.reduce(parsed, Yog.directed(), fn %{name: name, weight: weight}, g ->
        Model.add_node(g, name, weight)
      end)

    Enum.reduce(parsed, graph, fn %{name: name, branches: branches}, g ->
      Enum.reduce(branches, g, fn branch, g_acc ->
        Model.add_edge!(g_acc, name, branch, nil)
      end)
    end)
  end
end
