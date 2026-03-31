defmodule AdventOfCode.Y2020.Day07 do
  @moduledoc """
  --- Day 7: Handy Haversacks ---
  Problem Link: https://adventofcode.com/2020/day/7
  Difficulty: s
  Tags: graph-traversal digraph recursion
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Labeled
  alias Yog.Transform
  alias Yog.Traversal.Implicit

  def input, do: InputReader.read_from_file(2020, 7)

  def run(input \\ input()) do
    builder = parse(input)
    {solve_part_1(builder), solve_part_2(builder)}
  end

  defp solve_part_1(builder) do
    {:ok, shiny_gold_id} = Labeled.get_id(builder, "shiny gold")
    graph = Labeled.to_graph(builder)
    transposed = Transform.transpose(graph)

    Implicit.implicit_fold(
      from: shiny_gold_id,
      using: :breadth_first,
      initial: MapSet.new(),
      successors_of: fn id ->
        transposed
        |> Yog.successors(id)
        |> Enum.map(fn {nid, _} -> nid end)
      end,
      with: fn acc, node_id, _meta ->
        {:continue, MapSet.put(acc, node_id)}
      end
    )
    |> MapSet.size()
    |> Kernel.-(1)
  end

  defp solve_part_2(builder) do
    {:ok, shiny_gold_id} = Labeled.get_id(builder, "shiny gold")
    graph = Labeled.to_graph(builder)

    count_bags_inside(graph, shiny_gold_id)
  end

  defp count_bags_inside(graph, node_id) do
    successors = Yog.successors(graph, node_id)

    Enum.reduce(successors, 0, fn {child_id, count}, acc ->
      acc + count + count * count_bags_inside(graph, child_id)
    end)
  end

  defp parse(data) do
    lines = Transformers.lines(data)

    Enum.reduce(lines, Labeled.directed(), fn line, builder ->
      [parent, contents] = String.split(line, " bags contain ")

      contents
      |> String.replace(".", "")
      |> String.split(", ")
      |> Enum.reduce(builder, fn part, acc ->
        case parse_content_part(part) do
          {count, child} when count > 0 -> Labeled.add_edge(acc, parent, child, count)
          _ -> acc
        end
      end)
    end)
  end

  defp parse_content_part(part) do
    case String.split(part, " ") do
      [count_str, adj, color | _rest] ->
        case Integer.parse(count_str) do
          {count, _} -> {count, "#{adj} #{color}"}
          :error -> {0, ""}
        end

      _ ->
        {0, ""}
    end
  end
end
