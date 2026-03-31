defmodule AdventOfCode.Y2023.Day23 do
  @moduledoc """
  --- Day 23: A Long Walk ---
  Problem Link: https://adventofcode.com/2023/day/23
  Difficulty: xl
  Tags: graph longest-path
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  import Bitwise

  def input, do: InputReader.read_from_file(2023, 23)

  def run(input \\ input()) do
    {grid, _width, _height, start_pos, goal_pos} = parse(input)

    # Find intersections to compress the grid graph
    # Intersections are any points with >2 neighbors, plus the start and end.
    intersections = find_intersections(grid, start_pos, goal_pos)

    # Map each intersection to a unique ID for bitmasking (0 to N-1)
    node_to_id =
      intersections
      |> Enum.with_index()
      |> Enum.into(%{})

    start_id = node_to_id[start_pos]
    goal_id = node_to_id[goal_pos]

    # Run Part 1 and Part 2 in parallel
    task_1 =
      Task.async(fn ->
        graph = build_compressed_graph(grid, node_to_id, 1)
        solve(start_id, goal_id, graph)
      end)

    task_2 =
      Task.async(fn ->
        graph = build_compressed_graph(grid, node_to_id, 2)
        solve(start_id, goal_id, graph)
      end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  defp parse(data) do
    lines = data |> Transformers.lines()
    height = length(lines)
    width = lines |> List.first() |> String.length()

    grid =
      for {line, r} <- Enum.with_index(lines),
          {char, c} <- Enum.with_index(String.graphemes(line)),
          into: %{} do
        {{r, c}, char}
      end

    start_pos = {0, 1}
    goal_pos = {height - 1, width - 2}

    {grid, width, height, start_pos, goal_pos}
  end

  # Intersection points are where multiple paths meet or the start/goal.
  defp find_intersections(grid, start, goal) do
    for {pos, char} <- grid,
        char != "#",
        neighbors = get_neighbors(pos, grid, 2),
        length(neighbors) > 2 or pos == start or pos == goal do
      pos
    end
  end

  # Returns valid neighbors for a position.
  # Part 1 respects slopes (^ v < >), Part 2 treats them as regular paths.
  defp get_neighbors({r, c}, grid, part) do
    case {part, Map.get(grid, {r, c})} do
      {1, ">"} -> [{r, c + 1}]
      {1, "<"} -> [{r, c - 1}]
      {1, "v"} -> [{r + 1, c}]
      {1, "^"} -> [{r - 1, c}]
      _ -> [{r - 1, c}, {r + 1, c}, {r, c - 1}, {r, c + 1}]
    end
    |> Enum.filter(fn pos -> Map.get(grid, pos, "#") != "#" end)
  end

  # Builds a weighted graph where edges represent paths between intersections.
  defp build_compressed_graph(grid, node_to_id, part) do
    for {start_pos, start_id} <- node_to_id, into: %{} do
      edges =
        get_neighbors(start_pos, grid, part)
        |> Enum.flat_map(fn first_step ->
          case walk_to_intersection(first_step, start_pos, 1, grid, node_to_id, part) do
            {:ok, end_pos, dist} -> [{node_to_id[end_pos], dist}]
            :error -> []
          end
        end)

      {start_id, edges}
    end
  end

  # Walk down a corridor until we hit an intersection.
  defp walk_to_intersection(curr, prev, dist, grid, node_to_id, part) do
    if Map.has_key?(node_to_id, curr) do
      {:ok, curr, dist}
    else
      nexts =
        get_neighbors(curr, grid, part)
        |> Enum.reject(&(&1 == prev))

      case nexts do
        [next] -> walk_to_intersection(next, curr, dist + 1, grid, node_to_id, part)
        _ -> :error
      end
    end
  end

  # DFS to find the longest path from start to target.
  # uses a bitmask (visited) to ensure no node is visited twice.
  defp solve(start_id, target_id, graph) do
    dfs(start_id, target_id, bsl(1, start_id), 0, graph)
  end

  defp dfs(curr, target, _, cost, _) when curr == target, do: cost

  defp dfs(curr, target, visited, cost, graph) do
    neighbors = Map.get(graph, curr, [])

    Enum.reduce(neighbors, -1, fn {next_id, weight}, acc ->
      if band(visited, bsl(1, next_id)) == 0 do
        res = dfs(next_id, target, bor(visited, bsl(1, next_id)), cost + weight, graph)
        max(acc, res)
      else
        acc
      end
    end)
  end
end
