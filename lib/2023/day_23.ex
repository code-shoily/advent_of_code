defmodule AdventOfCode.Y2023.Day23 do
  @moduledoc """
  --- Day 23: A Long Walk ---
  Problem Link: https://adventofcode.com/2023/day/23
  Difficulty: xl
  Tags: graph longest-path slow refactor
  """
  alias AdventOfCode.Algorithms.Grid
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2023, 23)

  @start {0, 1}
  @up {-1, 0}
  @down {1, 0}
  @left {0, -1}
  @right {0, 1}
  @slopes %{@up => "^", @down => "v", @left => "<", @right => ">"}

  def run(input \\ input()) do
    input = parse(input)

    task_1 = Task.async(fn -> run_1(input) end)
    task_2 = Task.async(fn -> run_2(input) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def parse(data \\ input()) do
    grid = Grid.text_to_grid2d(data)
    {max_r, max_c} = Enum.max(Map.keys(grid))

    {{max_r, max_c - 1}, edges(grid, [{@start, @down}], [])}
  end

  defp run_1({{_, _} = finish, edges}) do
    edges
    |> make_graph()
    |> find_longest_path(@start, finish)
  end

  defp run_2({{_, _} = finish, edges}) do
    edges
    |> make_graph(:directed)
    |> find_longest_path(@start, finish)
  end

  defp edges(_, [], es), do: es

  defp edges(g, [{tile, dir} | todo], es) do
    {{tr, tc}, dist, ntodo} = follow_path(g, [{tile, dir}], tile, 0)
    ebgs = es |> Enum.map(fn {p, d, _, _} -> {p, d} end)
    nftodo = Enum.filter(ntodo, fn {p, d} -> {p, d} not in ebgs and {p, d} not in todo end)

    edges(g, todo ++ nftodo, [{tile, dir, {tr, tc}, dist} | es])
  end

  defp make_graph(edges, type \\ :undirected) do
    for {f, _, t, w} <- edges, reduce: Graph.new() do
      acc ->
        acc
        |> Graph.add_edge(f, t, weight: w)
        |> then(fn graph ->
          (type == :directed && Graph.add_edge(graph, t, f, weight: w)) || graph
        end)
    end
  end

  defp follow_path(g, [{tile, dir}], _, n) do
    next_tile = follow(tile, dir)

    if next_tile in Map.keys(g) do
      dirs =
        [@up, @down, @left, @right]
        |> List.delete(opposite(dir))
        |> Enum.map(fn d -> {d, g[follow(next_tile, d)]} end)
        |> Enum.filter(fn {_, c} -> c != "#" and c != nil end)

      dirs
      |> Enum.filter(fn {d, c} -> c == "." or c == @slopes[d] end)
      |> Enum.map(fn {d, _} -> {next_tile, d} end)
      |> then(fn dir ->
        (length(dirs) >= 2 && {next_tile, n + 1, dir}) ||
          follow_path(g, dir, next_tile, n + 1)
      end)
    else
      {tile, n, []}
    end
  end

  defp follow_path(_, dir, ppt, n), do: {ppt, n, dir}
  defp follow({a, b}, {c, d}), do: {a + c, b + d}
  defp opposite({a, b}), do: {-a, -b}

  defp path_length([a, b | rest], g, sum) do
    wt = g |> Graph.edge(a, b) |> Map.get(:weight, 0)
    path_length([b | rest], g, sum + wt)
  end

  defp path_length([_], _, sum), do: sum

  defp find_longest_path(graph, start, finish) do
    Graph.Pathfinding.all(graph, start, finish)
    |> Stream.map(&path_length(&1, graph, 0))
    |> Enum.max()
  end
end
