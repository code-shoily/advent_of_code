defmodule AdventOfCode.Y2018.Day07 do
  @moduledoc """
  --- Day 7: The Sum of Its Parts ---
  Problem Link: https://adventofcode.com/2018/day/7
  Difficulty: m
  Tags: graph simulation topological-sort
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Labeled
  alias Yog.Traversal

  def input, do: InputReader.read_from_file(2018, 7)

  def run(input \\ input()) do
    # Build the directed graph of dependencies.
    graph = parse(input)

    {run_1(graph), run_2(graph)}
  end

  defp run_1(graph) do
    # Part 1: Order of steps.
    # We must pick the alphabetically first step among all available ones.
    # Yog's lexicographical_topological_sort is built for exactly this!
    {:ok, ids} =
      Traversal.lexicographical_topological_sort(graph, fn a, b ->
        cond do
          a < b -> :lt
          a > b -> :gt
          true -> :eq
        end
      end)

    ids
    |> Enum.map_join("", fn id -> Yog.Model.node(graph, id) end)
  end

  defp run_2(graph) do
    # Part 2: Time to complete steps with 5 workers.
    # Each step duration: 60 + letter_value ('A'=1, etc.)
    all_nodes = Yog.Model.all_nodes(graph)

    # Initial in-degrees for dependency tracking
    in_degrees =
      all_nodes
      |> Map.new(fn id -> {id, length(Yog.Model.predecessors(graph, id))} end)

    # Initial available nodes (no dependencies)
    available =
      all_nodes
      |> Enum.filter(&(in_degrees[&1] == 0))
      |> Enum.sort_by(&Yog.Model.node(graph, &1))

    simulate(0, [], available, in_degrees, graph)
  end

  # Simulation loop: advances time and manages workers.
  defp simulate(time, [], [], _in_degrees, _graph), do: time

  defp simulate(time, workers, available, in_degrees, graph) do
    # Assign free workers (up to 5 total)
    num_free = 5 - length(workers)
    {to_assign, remaining_available} = Enum.split(available, num_free)

    new_workers =
      Enum.reduce(to_assign, workers, fn id, acc ->
        label = Yog.Model.node(graph, id)
        duration = 60 + hd(String.to_charlist(label)) - ?A + 1
        [{id, duration} | acc]
      end)

    # Find the nearest event (finish of a worker)
    dt = Enum.min_by(new_workers, fn {_, t} -> t end) |> elem(1)

    # Advance time!
    # Update worker timers and separate finishing ones
    {finishing, busy} =
      new_workers
      |> Enum.map(fn {id, t} -> {id, t - dt} end)
      |> Enum.split_with(fn {_, t} -> t == 0 end)

    # When a step finishes, we decrease the in-degree of its successors
    {new_in_degrees, newly_available} =
      Enum.reduce(finishing, {in_degrees, []}, fn {id, _}, {degs, acc_new} ->
        successors = Yog.Model.successors(graph, id) |> Enum.map(&elem(&1, 0))

        Enum.reduce(successors, {degs, acc_new}, fn succ, {d_acc, a_acc} ->
          new_deg = d_acc[succ] - 1
          new_d_acc = Map.put(d_acc, succ, new_deg)
          new_a_acc = if new_deg == 0, do: [succ | a_acc], else: a_acc
          {new_d_acc, new_a_acc}
        end)
      end)

    # Finalize the next iteration's available nodes (must be sorted alphabetically)
    final_available =
      (remaining_available ++ newly_available)
      |> Enum.sort_by(&Yog.Model.node(graph, &1))

    simulate(time + dt, busy, final_available, new_in_degrees, graph)
  end

  def parse(data \\ input()) do
    # Build a directed graph from the dependency strings.
    Transformers.lines(data)
    |> Enum.reduce(Labeled.directed(), fn line, builder ->
      # Example: "Step C must be finished before step P can begin."
      words = String.split(line, " ")
      prereq = Enum.at(words, 1)
      step = Enum.at(words, 7)
      Labeled.add_unweighted_edge(builder, prereq, step)
    end)
    |> Labeled.to_graph()
  end
end
