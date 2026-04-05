defmodule AdventOfCode.Y2021.Day15 do
  @moduledoc """
  --- Day 15: Chiton ---
  Problem Link: https://adventofcode.com/2021/day/15
  Difficulty: l
  Tags: graph
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Grid
  alias Yog.Pathfinding.{AStar, Dijkstra}

  def input, do: InputReader.read_from_file(2021, 15)

  def run(input \\ input()) do
    input_grid = parse(input)

    task_1 = Task.async(fn -> solve_part_1(input_grid) end)
    task_2 = Task.async(fn -> solve_part_2(input_grid) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      line
      |> String.graphemes()
      |> Enum.map(&String.to_integer/1)
    end)
  end

  defp solve_part_1(grid_data) do
    grid = Grid.from_2d_list(grid_data, :directed, Grid.always())
    graph = Grid.to_graph(grid)
    rows = grid.rows
    cols = grid.cols

    # Weights are the target node's data (the risk level of the cell entered)
    graph =
      Yog.Transform.map_edges_indexed(graph, fn _src, dst, _weight ->
        Yog.Model.node(graph, dst)
      end)

    {:ok, path} = Dijkstra.shortest_path(graph, 0, rows * cols - 1)
    path.weight
  end

  defp solve_part_2(grid_data) do
    rows = length(grid_data)
    cols = length(hd(grid_data))

    # Pre-calculate base values in a tuple for O(1) access during expansion
    base_grid = grid_data |> Enum.map(&List.to_tuple/1) |> List.to_tuple()

    big_grid_data =
      for r <- 0..(rows * 5 - 1) do
        for c <- 0..(cols * 5 - 1) do
          base_r = rem(r, rows)
          base_c = rem(c, cols)
          offset = div(r, rows) + div(c, cols)

          val = elem(elem(base_grid, base_r), base_c) + offset
          if val > 9, do: val - 9, else: val
        end
      end

    grid = Grid.from_2d_list(big_grid_data, :directed, Grid.always())
    graph = Grid.to_graph(grid)
    big_rows = grid.rows
    big_cols = grid.cols

    # Weights are the target node's data
    graph =
      Yog.Transform.map_edges_indexed(graph, fn _src, dst, _weight ->
        Yog.Model.node(graph, dst)
      end)

    # Manhattan distance heuristic for A*
    heuristic = fn node_id, goal_id ->
      {r1, c1} = {div(node_id, big_cols), rem(node_id, big_cols)}
      {r2, c2} = {div(goal_id, big_cols), rem(goal_id, big_cols)}
      abs(r1 - r2) + abs(c1 - c2)
    end

    {:ok, path} = AStar.a_star(graph, 0, big_rows * big_cols - 1, heuristic)
    path.weight
  end
end
