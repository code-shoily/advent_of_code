defmodule AdventOfCode.Y2016.Day13 do
  @moduledoc """
  --- Day 13: A Maze of Twisty Little Cubicles ---
  Problem Link: https://adventofcode.com/2016/day/13
  Difficulty: m
  Tags: graph implicit-graph shortest-path a-star bfs
  """
  import Bitwise
  alias AdventOfCode.Helpers.InputReader
  alias Yog.Pathfinding.AStar
  alias Yog.Traversal

  def input, do: InputReader.read_from_file(2016, 13)

  def run(input \\ input()) do
    fav = parse(input)

    {run_1(fav), run_2(fav)}
  end

  defp run_1(fav) do
    # Target coordinate: (31, 39)
    # Finding shortest path using A* search from (1, 1).
    target = {31, 39}

    successors = fn pos ->
      open_neighbors(pos, fav) |> Enum.map(fn n -> {n, 1} end)
    end

    heuristic = fn {x, y} ->
      abs(x - target_x(target)) + abs(y - target_y(target))
    end

    {:ok, dist} =
      AStar.implicit_a_star(
        from: {1, 1},
        is_goal: fn pos -> pos == target end,
        successors_with_cost: successors,
        heuristic: heuristic
      )

    dist
  end

  defp run_2(fav) do
    # BFS to find all unique nodes reachable within 50 steps.
    Traversal.implicit_fold(
      from: {1, 1},
      using: :breadth_first,
      initial: MapSet.new(),
      successors_of: fn pos -> open_neighbors(pos, fav) end,
      with: fn acc, pos, meta ->
        new_acc = MapSet.put(acc, pos)

        # Stop exploring depth > 50.
        action = if meta.depth >= 50, do: :stop, else: :continue
        {action, new_acc}
      end
    )
    |> MapSet.size()
  end

  defp is_wall?(x, y, fav) do
    if x < 0 or y < 0 do
      true
    else
      val = x * x + 3 * x + 2 * x * y + y + y * y + fav
      rem(count_ones(val), 2) != 0
    end
  end

  defp open_neighbors({x, y}, fav) do
    [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
    |> Enum.filter(fn {nx, ny} -> not is_wall?(nx, ny, fav) end)
  end

  defp count_ones(0), do: 0
  defp count_ones(n), do: (n &&& 1) + count_ones(n >>> 1)

  defp target_x({x, _}), do: x
  defp target_y({_, y}), do: y

  def parse(data \\ input()) do
    data |> String.trim() |> String.trim_trailing(".") |> String.to_integer()
  end
end
