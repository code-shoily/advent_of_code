defmodule AdventOfCode.Y2023.Day25 do
  @moduledoc """
  --- Day 25: Snowverload ---
  Problem Link: https://adventofcode.com/2023/day/25
  Difficulty: xl
  Tags: graph min-cut probabilistic refactor not-fast-enough
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 25)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    Stream.repeatedly(fn -> contract(input) end)
    |> Enum.find(fn g1 -> cut_size(g1, input) == 3 end)
    |> Map.keys()
    |> Enum.map(&String.length/1)
    |> Enum.map(&div(&1, 3))
    |> Enum.product()
  end

  defp run_2(_), do: "🎉"

  def parse(input) do
    for line <- Transformers.lines(input), reduce: %{} do
      graph ->
        [u | connected] = String.split(line, [":", " "], trim: true)

        for v <- connected, reduce: graph do
          edges ->
            edges
            |> Map.update(u, MapSet.new([v]), &MapSet.put(&1, v))
            |> Map.update(v, MapSet.new([u]), &MapSet.put(&1, u))
        end
    end
  end

  def cut_size(g, h) do
    for {key, _} <- g do
      key
      |> String.to_charlist()
      |> Enum.chunk_every(3)
      |> Enum.map(&to_string/1)
    end
    |> then(fn [us, vs] ->
      for u <- us, _ <- MapSet.intersection(h[u], MapSet.new(vs)), reduce: 0 do
        acc -> acc + 1
      end
    end)
  end

  def contract(graph) when map_size(graph) == 2, do: graph

  def contract(graph) do
    {u, u_edges} = Enum.random(graph)
    v = Enum.random(u_edges)

    u_edges = u_edges |> MapSet.delete(v)
    v_edges = graph[v] |> MapSet.delete(u)
    u_v_edges = MapSet.union(u_edges, v_edges)
    uv = u <> v

    Enum.reduce(u_edges, graph, fn c, acc ->
      Map.update!(acc, c, &MapSet.put(MapSet.delete(&1, u), uv))
    end)
    |> then(fn graph ->
      Enum.reduce(v_edges, graph, fn c, acc ->
        Map.update!(acc, c, &MapSet.put(MapSet.delete(&1, v), uv))
      end)
      |> Map.drop([u, v])
      |> Map.put(uv, u_v_edges)
    end)
    |> contract()
  end
end
