defmodule AdventOfCode.Y2019.Day18 do
  @moduledoc """
  --- Day 18: Many-Worlds Interpretation ---
  Problem Link: https://adventofcode.com/2019/day/18
  Difficulty: xl
  Tags: graph dijkstra implicit-graph bitmask state-space-search
  """
  import Bitwise
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Pathfinding.Dijkstra

  def input, do: InputReader.read_from_file(2019, 18)

  def run(input \\ input()) do
    lines = Transformers.lines(input)

    p1 = solve_part_1(lines)
    p2 = solve_part_2(lines)

    {p1, p2}
  end

  # Part 1: Single robot
  defp solve_part_1(lines) do
    grid = parse_grid(lines)
    pois = find_pois(grid, ["@"])
    all_keys_mask = calculate_keys_mask(pois)
    adj = build_poi_graph(grid, pois)

    start_id = "@"

    Dijkstra.implicit_dijkstra(
      from: {start_id, 0},
      successors_with_cost: fn {at, collected} ->
        edges = Map.get(adj, at, [])

        edges
        |> Enum.filter(fn edge ->
          band(collected, edge.required) == edge.required and
            band(collected, key_bit(edge.to)) == 0
        end)
        |> Enum.map(fn edge ->
          new_collected = bor(collected, key_bit(edge.to))
          {{edge.to, new_collected}, edge.dist}
        end)
      end,
      is_goal: fn {_, collected} -> collected == all_keys_mask end,
      zero: 0,
      add: &Kernel.+/2,
      compare: &Yog.Utils.compare/2
    )
    |> case do
      {:ok, dist} -> dist
      :error -> :failed
    end
  end

  # Part 2: Four robots
  defp solve_part_2(lines) do
    grid = parse_grid(lines)
    grid = modify_for_part_2(grid)

    starts = ["1", "2", "3", "4"]
    pois = find_pois_part_2(grid, starts)
    all_keys_mask = calculate_keys_mask(pois)
    adj = build_poi_graph(grid, pois)

    initial_robots = starts

    Dijkstra.implicit_dijkstra(
      from: {initial_robots, 0},
      successors_with_cost: fn {robots, collected} ->
        Enum.with_index(robots)
        |> Enum.flat_map(fn {at, idx} ->
          edges = Map.get(adj, at, [])

          edges
          |> Enum.filter(fn edge ->
            band(collected, edge.required) == edge.required and
              band(collected, key_bit(edge.to)) == 0
          end)
          |> Enum.map(fn edge ->
            new_collected = bor(collected, key_bit(edge.to))
            new_robots = List.replace_at(robots, idx, edge.to)
            {{new_robots, new_collected}, edge.dist}
          end)
        end)
      end,
      visited_by: fn {robots, collected} -> {robots, collected} end,
      is_goal: fn {_, collected} -> collected == all_keys_mask end,
      zero: 0,
      add: &Kernel.+/2,
      compare: &Yog.Utils.compare/2
    )
    |> case do
      {:ok, dist} -> dist
      :error -> :failed
    end
  end

  # --- Grid Utilities ---

  defp parse_grid(lines) do
    for {line, y} <- Enum.with_index(lines),
        {char, x} <- Enum.with_index(String.graphemes(line)),
        into: %{},
        do: {{x, y}, char}
  end

  defp find_pois(grid, start_chars) do
    grid
    |> Enum.filter(fn {_, char} -> char in start_chars or key?(char) end)
    |> Map.new(fn {pos, char} -> {char, pos} end)
  end

  defp find_pois_part_2(grid, start_chars) do
    grid
    |> Enum.filter(fn {_, char} ->
      char in start_chars or key?(char)
    end)
    |> Map.new(fn {pos, char} -> {char, pos} end)
  end

  defp calculate_keys_mask(pois) do
    Enum.reduce(pois, 0, fn {label, _}, acc ->
      if key?(label), do: bor(acc, key_bit(label)), else: acc
    end)
  end

  defp build_poi_graph(grid, pois) do
    Map.new(pois, fn {label, pos} ->
      {label, find_reachable_from(grid, pos)}
    end)
  end

  defp find_reachable_from(grid, start_pos) do
    q = :queue.in({start_pos, 0, 0}, :queue.new())
    visited = MapSet.new([start_pos])
    bfs_poi(grid, q, visited, [])
  end

  # BFS that finds ALL reachable keys, tracking both doors AND keys as requirements
  defp bfs_poi(grid, q, visited, acc) do
    case :queue.out(q) do
      {:empty, _} ->
        acc

      {{:value, {pos, dist, mask}}, rest} ->
        char = grid[pos]

        # Record this key if we found one (but don't stop exploring!)
        new_acc =
          if dist > 0 and key?(char) do
            [%{to: char, dist: dist, required: mask} | acc]
          else
            acc
          end

        # Continue exploring - update mask with doors AND keys we pass through
        new_mask =
          cond do
            door?(char) -> bor(mask, door_bit(char))
            key?(char) -> bor(mask, key_bit(char))
            true -> mask
          end

        {nq, nv} =
          Enum.reduce(neighbors(pos), {rest, visited}, fn nb, {q_acc, v_acc} ->
            nb_char = Map.get(grid, nb)

            if nb_char != nil and nb_char != "#" and not MapSet.member?(v_acc, nb) do
              {:queue.in({nb, dist + 1, new_mask}, q_acc), MapSet.put(v_acc, nb)}
            else
              {q_acc, v_acc}
            end
          end)

        bfs_poi(grid, nq, nv, new_acc)
    end
  end

  defp neighbors({x, y}), do: [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]

  defp key?(char), do: char != nil and char >= "a" and char <= "z"
  defp door?(char), do: char != nil and char >= "A" and char <= "Z"

  defp key_bit(char), do: bsl(1, hd(String.to_charlist(char)) - ?a)
  defp door_bit(char), do: bsl(1, hd(String.to_charlist(char)) - ?A)

  defp modify_for_part_2(grid) do
    start_item = Enum.find(grid, fn {_, v} -> v == "@" end)

    if start_item do
      {cx, cy} = elem(start_item, 0)

      grid
      |> Map.merge(%{
        {cx, cy} => "#",
        {cx + 1, cy} => "#",
        {cx - 1, cy} => "#",
        {cx, cy + 1} => "#",
        {cx, cy - 1} => "#",
        {cx - 1, cy - 1} => "1",
        {cx + 1, cy - 1} => "2",
        {cx - 1, cy + 1} => "3",
        {cx + 1, cy + 1} => "4"
      })
    else
      grid
    end
  end

  def parse(data \\ input()), do: data
end
