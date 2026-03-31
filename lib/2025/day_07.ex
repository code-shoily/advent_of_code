defmodule AdventOfCode.Y2025.Day07 do
  @moduledoc """
  --- Day 7: Laboratories ---
  Problem Link: https://adventofcode.com/2025/day/7
  Difficulty: l
  Tags: grid dynamic-programming
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog
  alias Yog.Traversal.{Sort, Walk}

  def input, do: InputReader.read_from_file(2025, 7)

  @spec run(binary()) :: {non_neg_integer(), any()}
  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  def parse(data \\ input()) do
    lines = Transformers.lines(data)

    for {line, r} <- Enum.with_index(lines),
        {char, c} <- Enum.with_index(String.graphemes(line)),
        into: %{} do
      {{r, c}, char}
    end
  end

  def run_1(grid) do
    graph = build_graph(grid)
    start = find_start(grid)

    reached =
      Walk.fold_walk(
        over: graph,
        from: start,
        using: :breadth_first,
        initial: MapSet.new(),
        with: fn acc, node, _meta ->
          {:continue, MapSet.put(acc, node)}
        end
      )

    reached
    |> Enum.count(fn
      {r, c} -> Map.get(grid, {r, c}) == "^"
      _ -> false
    end)
  end

  def run_2(grid) do
    graph = build_graph(grid)
    start = find_start(grid)
    max_r = get_max_r(grid)

    case Sort.topological_sort(graph) do
      {:ok, sorted} ->
        counts =
          Enum.reduce(sorted, %{start => 1}, fn node, acc ->
            case Map.get(acc, node) do
              nil ->
                acc

              count ->
                Yog.successor_ids(graph, node)
                |> Enum.reduce(acc, fn succ, inner_acc ->
                  Map.update(inner_acc, succ, count, &(&1 + count))
                end)
            end
          end)

        counts
        |> Enum.filter(fn {{r, _}, _} -> r > max_r end)
        |> Enum.reduce(0, fn {_, n}, sum -> sum + n end)

      {:error, :contains_cycle} ->
        0
    end
  end

  defp get_max_r(grid), do: grid |> Map.keys() |> Enum.map(fn {r, _} -> r end) |> Enum.max()

  defp find_start(grid) do
    Enum.find_value(grid, fn {pos, char} ->
      if char == "S", do: pos
    end)
  end

  defp build_graph(grid) do
    Enum.reduce(grid, Yog.directed(), fn {{r, c}, char}, graph ->
      case char do
        "^" ->
          graph
          |> Yog.add_edge_ensure({r, c}, {r + 1, c - 1}, 1, char)
          |> Yog.add_edge_ensure({r, c}, {r + 1, c + 1}, 1, char)

        _ ->
          Yog.add_edge_ensure(graph, {r, c}, {r + 1, c}, 1, char)
      end
    end)
  end
end
