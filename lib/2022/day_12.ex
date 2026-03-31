defmodule AdventOfCode.Y2022.Day12 do
  @moduledoc """
  --- Day 12: Hill Climbing Algorithm ---
  Problem Link: https://adventofcode.com/2022/day/12
  Difficulty: m
  Tags: graph graph-traversal
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.{Grid, GridGraph}
  alias Yog.Pathfinding.Dijkstra
  alias Yog.Transform

  def input, do: InputReader.read_from_file(2022, 12)

  def run(input \\ input()) do
    {grid, start_pos, end_pos} = parse(input)

    # Hill-climbing rule: can only step to a cell at most 1 unit higher,
    # but can drop any distance.
    builder = Grid.from_2d_list(grid, :directed, fn from, to -> to - from <= 1 end)
    graph = Grid.to_graph(builder)

    {sr, sc} = start_pos
    {er, ec} = end_pos
    start_id = GridGraph.coord_to_id(builder, sr, sc)
    end_id = GridGraph.coord_to_id(builder, er, ec)

    # Part 1 and Part 2 are computed in parallel
    task_1 =
      Task.async(fn ->
        case Dijkstra.shortest_path(graph, start_id, end_id) do
          {:ok, path} -> path.weight
          _ -> :no_path
        end
      end)

    task_2 =
      Task.async(fn ->
        # Find the shortest path from any node with elevation 0 ('a') to 'E'.
        # Efficient approach: transpose the graph and find distances FROM E to ALL nodes.
        transposed = Transform.transpose(graph)
        distances = Dijkstra.single_source_distances(transposed, end_id)

        # Elevation is stored as node data in the grid-builder graph
        graph.nodes
        |> Enum.filter(fn {id, elevation} -> elevation == 0 and Map.has_key?(distances, id) end)
        |> Enum.map(fn {id, _} -> Map.get(distances, id) end)
        |> Enum.min()
      end)

    {Task.await(task_1), Task.await(task_2)}
  end

  defp parse(data) do
    lines = Transformers.lines(data)
    grid = Enum.map(lines, &String.graphemes/1)

    {start_pos, end_pos} = find_points(grid)

    elevations =
      Enum.map(grid, fn row ->
        Enum.map(row, fn
          "S" -> 0
          "E" -> 25
          char -> :binary.first(char) - ?a
        end)
      end)

    {elevations, start_pos, end_pos}
  end

  defp find_points(grid) do
    points =
      for {row_list, r} <- Enum.with_index(grid),
          {char, c} <- Enum.with_index(row_list),
          char in ["S", "E"],
          into: %{} do
        {char, {r, c}}
      end

    {points["S"], points["E"]}
  end
end
