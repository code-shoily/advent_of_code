defmodule AdventOfCode.Y2024.Day05 do
  @moduledoc """
  --- Day 5: Print Queue ---
  Problem Link: https://adventofcode.com/2024/day/5
  Difficulty: xs
  Tags: graph sort topological-sort
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Traversal

  def input, do: InputReader.read_from_file(2024, 5)

  def run(input \\ input()) do
    {rules, updates} = parse(input)

    # Process all updates based on the rules.
    # For each update, we produce a correctly ordered version using topological sort.
    processed =
      for update <- updates do
        reordered = reorder(update, rules)
        {update, reordered}
      end

    {run_1(processed), run_2(processed)}
  end

  defp run_1(processed) do
    processed
    |> Enum.filter(fn {original, reordered} -> original == reordered end)
    |> Enum.sum_by(fn {reordered, _} -> get_middle(reordered) end)
  end

  defp run_2(processed) do
    processed
    |> Enum.filter(fn {original, reordered} -> original != reordered end)
    |> Enum.sum_by(fn {_, reordered} -> get_middle(reordered) end)
  end

  defp get_middle(list) do
    Enum.at(list, div(length(list), 2))
  end

  def parse(data \\ input()) do
    [rules_raw, updates_raw] = Transformers.sections(data)
    {parse_rules(rules_raw), parse_updates(updates_raw)}
  end

  defp parse_rules(raw) do
    for line <- Transformers.lines(raw) do
      [a, b] = String.split(line, "|") |> Enum.map(&String.to_integer/1)
      {a, b}
    end
  end

  defp parse_updates(raw) do
    for line <- Transformers.lines(raw) do
      line |> String.split(",") |> Enum.map(&String.to_integer/1)
    end
  end

  # Reorder an update by building a dependency graph and performing a topological sort.
  defp reorder(update, rules) do
    update_set = MapSet.new(update)

    # Filter rules that only apply to the current set of pages.
    active_rules =
      Enum.filter(rules, fn {a, b} ->
        MapSet.member?(update_set, a) and MapSet.member?(update_set, b)
      end)

    # Build a directed graph of dependencies.
    graph =
      Enum.reduce(active_rules, Yog.directed(), fn {a, b}, acc ->
        Yog.add_edge_ensure(acc, a, b, 1, nil)
      end)

    # Ensure all pages in the update are represented in the graph.
    graph =
      Enum.reduce(update, graph, fn page, acc ->
        if Yog.has_node?(acc, page) do
          acc
        else
          Yog.add_node(acc, page, nil)
        end
      end)

    # Correct sequence is the topological order of these dependencies.
    case Traversal.topological_sort(graph) do
      {:ok, sorted} -> sorted
      {:error, :contains_cycle} -> update
    end
  end
end
