defmodule AdventOfCode.Y2023.Day25 do
  @moduledoc """
  --- Day 25: Snowverload ---
  Problem Link: https://adventofcode.com/2023/day/25
  Difficulty: l
  Tags: graph min-cut
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Labeled
  alias Yog.Flow.MinCut
  alias Yog.Flow.MinCutResult

  def input, do: InputReader.read_from_file(2023, 25)

  def run(input \\ input()) do
    builder = parse(input)
    graph = Labeled.to_graph(builder)

    {run_1(graph), run_2(graph)}
  end

  defp run_1(graph) do
    # Stoer-Wagner finds the global minimum cut in an undirected graph.
    # In this problem, we are guaranteed that the global min-cut size is 3.
    result = MinCut.global_min_cut(graph)

    # Use the optimized partition_product helper
    MinCutResult.partition_product(result)
  end

  defp run_2(_), do: "🎉"

  @doc """
  Parses the input into a Yog Labeled builder.
  Each line defines a node and its connections: `jqt: rhn xhk nvd`
  """
  def parse(input) do
    data = Transformers.lines(input)

    Enum.reduce(data, Labeled.undirected(), fn line, builder ->
      case String.split(line, ": ", trim: true) do
        [source, dests] ->
          dests
          |> String.split(" ", trim: true)
          |> Enum.reduce(builder, fn target, b ->
            Labeled.add_edge(b, source, target, 1)
          end)

        _ ->
          builder
      end
    end)
  end
end
