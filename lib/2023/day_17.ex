defmodule AdventOfCode.Y2023.Day17 do
  @moduledoc """
  --- Day 17: Clumsy Crucible ---
  Problem Link: https://adventofcode.com/2023/day/17
  Difficulty: hard
  Tags: graph dijkstra shortest-path state-space grid
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Pathfinding.Dijkstra

  def input, do: InputReader.read_from_file(2023, 17)

  def run(input \\ input()) do
    grid = parse_grid(input)
    {max_x, max_y} = grid |> Map.keys() |> Enum.max()

    p1 = solve(grid, max_x, max_y, 0, 3)
    p2 = solve(grid, max_x, max_y, 4, 10)

    {p1, p2}
  end

  defp solve(grid, max_x, max_y, min_turn, max_straight) do
    # State: {x, y, dir_atom, consecutive_count}
    # dir_atom: :r, :l, :d, :u, :start
    Dijkstra.implicit_dijkstra_by(
      from: {0, 0, :start, 0},
      successors_with_cost: fn {x, y, dir, count} ->
        # Possible directions to try
        dirs = if dir == :start, do: [:r, :d], else: next_dirs(dir, count, min_turn, max_straight)

        Enum.flat_map(dirs, fn d ->
          {nx, ny} = move(x, y, d)

          case Map.fetch(grid, {nx, ny}) do
            {:ok, cost} ->
              # New count is count + 1 if straight, else 1
              ncount = if d == dir, do: count + 1, else: 1
              [{{nx, ny, d, ncount}, cost}]

            :error ->
              []
          end
        end)
      end,
      is_goal: fn {x, y, _, count} ->
        x == max_x and y == max_y and count >= min_turn
      end,
      visited_by: fn state -> state end
    )
    |> case do
      {:ok, dist} -> dist
      _ -> :failed
    end
  end

  defp next_dirs(dir, count, min_turn, max_straight) do
    # Rules:
    # 1. Can always go straight if count < max_straight
    # 2. Can turn if count >= min_turn
    # 3. NO REVERSE

    # Straight?
    straight = if count < max_straight, do: [dir], else: []
    # Turns?
    turns = if count >= min_turn, do: turns(dir), else: []

    straight ++ turns
  end

  defp turns(:r), do: [:u, :d]
  defp turns(:l), do: [:u, :d]
  defp turns(:u), do: [:l, :r]
  defp turns(:d), do: [:l, :r]

  defp move(x, y, :r), do: {x + 1, y}
  defp move(x, y, :l), do: {x - 1, y}
  defp move(x, y, :u), do: {x, y - 1}
  defp move(x, y, :d), do: {x, y + 1}

  defp parse_grid(input) do
    input
    |> Transformers.lines()
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {line, y}, grid ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.reduce(grid, fn {char, x}, acc ->
        Map.put(acc, {x, y}, String.to_integer(char))
      end)
    end)
  end

  def parse(data \\ input()), do: data
end
