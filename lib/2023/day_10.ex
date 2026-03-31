defmodule AdventOfCode.Y2023.Day10 do
  @moduledoc """
  --- Day 10: Pipe Maze ---
  Problem Link: https://adventofcode.com/2023/day/10
  Difficulty: xl
  Tags: graph bfs grid
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Grid
  alias Yog.Pathfinding.Dijkstra

  def input, do: InputReader.read_from_file(2023, 10)

  def run(input \\ input()) do
    input_cells = parse(input)

    # Use Grid builder to create the graph based on pipe connectivity
    grid_graph = Grid.from_2d_list(input_cells, :directed, &can_connect/2)
    graph = grid_graph.graph

    # Find the starting node 'S'
    {start_id, _} = Enum.find(graph.nodes, fn {_, cell} -> cell.char == "S" end)

    # Calculate distances from start to find farthest point (Part 1) and identify circuit (Part 2)
    distances = Dijkstra.single_source_distances(graph, start_id)

    part_1 = distances |> Map.values() |> Enum.max()

    # Part 2: Number of enclosed points using scan-line parity
    part_2 = solve_part_2(grid_graph, start_id, distances)

    {part_1, part_2}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.with_index()
    |> Enum.map(fn {line, r} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {char, c} ->
        %{row: r, col: c, char: char}
      end)
    end)
  end

  # Logic for pipe connectivity: only cardinal moves are allowed if pipes match
  defp can_connect(from, to) do
    dr = to.row - from.row
    dc = to.col - from.col

    valid_out =
      case from.char do
        "|" -> dr != 0 and dc == 0
        "-" -> dr == 0 and dc != 0
        "L" -> (dr == -1 and dc == 0) or (dr == 0 and dc == 1)
        "J" -> (dr == -1 and dc == 0) or (dr == 0 and dc == -1)
        "7" -> (dr == 1 and dc == 0) or (dr == 0 and dc == -1)
        "F" -> (dr == 1 and dc == 0) or (dr == 0 and dc == 1)
        "S" -> true
        "." -> false
        _ -> false
      end

    valid_in =
      case to.char do
        "|" -> dr != 0 and dc == 0
        "-" -> dr == 0 and dc != 0
        "L" -> (dr == 1 and dc == 0) or (dr == 0 and dc == -1)
        "J" -> (dr == 1 and dc == 0) or (dr == 0 and dc == 1)
        "7" -> (dr == -1 and dc == 0) or (dr == 0 and dc == 1)
        "F" -> (dr == -1 and dc == 0) or (dr == 0 and dc == -1)
        "S" -> true
        "." -> false
        _ -> false
      end

    valid_out and valid_in
  end

  defp solve_part_2(grid_graph, start_id, distances) do
    rows = grid_graph.rows
    cols = grid_graph.cols
    graph = grid_graph.graph

    # Isolate the main loop to avoid junk branches that could interfere with parity logic
    loop_nodes = isolate_loop(Map.keys(distances), graph)

    # Identify what pipe character 'S' represents based on its loop neighbors
    # This helps in the scan-line crossing logic.
    s_neighbors =
      Yog.Model.successors(graph, start_id)
      |> Enum.map(fn {nid, _} -> nid end)
      |> Enum.filter(&MapSet.member?(loop_nodes, &1))

    # Get coordinates of S and its neighbors to determine the pipe type
    {sr, sc} = Yog.Builder.GridGraph.id_to_coord(grid_graph, start_id)

    s_offsets =
      Enum.map(s_neighbors, fn nid ->
        {nr, nc} = Yog.Builder.GridGraph.id_to_coord(grid_graph, nid)
        {nr - sr, nc - sc}
      end)
      |> MapSet.new()

    s_char =
      case {MapSet.member?(s_offsets, {-1, 0}), MapSet.member?(s_offsets, {1, 0}),
            MapSet.member?(s_offsets, {0, -1}), MapSet.member?(s_offsets, {0, 1})} do
        {true, true, _, _} -> "|"
        {_, _, true, true} -> "-"
        {true, _, _, true} -> "L"
        {true, _, true, _} -> "J"
        {_, true, true, _} -> "7"
        {_, true, _, true} -> "F"
        _ -> "S"
      end

    # Scan each row and count points where the loop has been crossed an odd number of times
    for r <- 0..(rows - 1), reduce: 0 do
      total_count ->
        {row_enclosed, _inside} =
          for c <- 0..(cols - 1), reduce: {0, false} do
            {count, inside} ->
              id = Yog.Builder.GridGraph.coord_to_id(grid_graph, r, c)

              if MapSet.member?(loop_nodes, id) do
                # Part of the loop - check if it's a boundary crossing
                cell = Yog.Model.node(graph, id)
                char = if cell.char == "S", do: s_char, else: cell.char

                # Standard scan-line crossing logic: | L J flip whereas F 7 - do not
                # (This effectively counts crossings of a line slightly above the center)
                if char in ["|", "L", "J"] do
                  {count, not inside}
                else
                  {count, inside}
                end
              else
                # Empty space or irrelevant pipe - count if inside
                if inside, do: {count + 1, inside}, else: {count, inside}
              end
          end

        total_count + row_enclosed
    end
  end

  defp isolate_loop(nodes, graph) do
    node_set = MapSet.new(nodes)
    prune_branches(node_set, graph)
  end

  defp prune_branches(node_set, graph) do
    to_remove =
      Enum.filter(node_set, fn id ->
        degree =
          Yog.Model.successors(graph, id)
          |> Enum.count(fn {neighbor, _} -> MapSet.member?(node_set, neighbor) end)

        degree < 2
      end)

    if to_remove == [] do
      node_set
    else
      new_set = Enum.reduce(to_remove, node_set, fn id, acc -> MapSet.delete(acc, id) end)
      prune_branches(new_set, graph)
    end
  end
end
