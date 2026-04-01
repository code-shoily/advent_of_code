defmodule AdventOfCode.Y2022.Day24 do
  @moduledoc """
  --- Day 24: Blizzard Basin ---
  Problem Link: https://adventofcode.com/2022/day/24
  Difficulty: m
  Tags: graph implicit-graph shortest-path a-star
  """
  alias AdventOfCode.Helpers.InputReader
  alias Yog.Pathfinding.AStar

  def input, do: InputReader.read_from_file(2022, 24)

  def run(input \\ input()) do
    {up, down, left, right, w, h} = parse(input)

    inner_w = w - 2
    inner_h = h - 2
    cycle = lcm(inner_w, inner_h)

    start_pos = {1, 0}
    goal_pos = {inner_w, h - 1}
    {:ok, t1} = solve_path(start_pos, goal_pos, 0, up, down, left, right, w, h, cycle)
    {:ok, t2} = solve_path(goal_pos, start_pos, t1, up, down, left, right, w, h, cycle)
    {:ok, t3} = solve_path(start_pos, goal_pos, t2, up, down, left, right, w, h, cycle)

    {t1, t3}
  end

  # credo:disable-next-line Credo.Check.Refactor.FunctionArity
  defp solve_path({sx, sy}, {gx, gy}, start_t, up, down, left, right, w, h, cycle) do
    inner_w = w - 2
    inner_h = h - 2

    is_valid = fn x, y, t ->
      cond do
        {x, y} == {sx, sy} -> true
        {x, y} == {gx, gy} -> true
        x <= 0 or x > inner_w or y <= 0 or y > inner_h -> false
        true -> not has_blizzard?(x, y, t, up, down, left, right, inner_w, inner_h, w)
      end
    end

    successors = fn {x, y, t} ->
      nt = t + 1

      [{x, y}, {x, y - 1}, {x, y + 1}, {x - 1, y}, {x + 1, y}]
      |> Enum.filter(fn {nx, ny} -> is_valid.(nx, ny, nt) end)
      |> Enum.map(fn {nx, ny} -> {{nx, ny, nt}, 1} end)
    end

    visited_by = fn {x, y, t} -> {x, y, rem(t, cycle)} end
    is_goal = fn {x, y, _t} -> {x, y} == {gx, gy} end
    heuristic = fn {x, y, _t} -> abs(gx - x) + abs(gy - y) end

    AStar.implicit_a_star_by(
      from: {sx, sy, start_t},
      successors_with_cost: successors,
      visited_by: visited_by,
      is_goal: is_goal,
      heuristic: heuristic
    )
    |> case do
      {:ok, cost} -> {:ok, start_t + cost}
      :error -> :error
    end
  end

  defp has_blizzard?(x, y, t, up, down, left, right, iw, ih, w) do
    # At time t, a tile (x, y) is occupied if:
    # An '^' blizzard was at (x, (y-1+t) % ih + 1)
    # A 'v' blizzard was at (x, (y-1-t) % ih + 1)
    # A '<' blizzard was at ((x-1+t) % iw + 1, y)
    # A '>' blizzard was at ((x-1-t) % iw + 1, y)

    uy = rem_euclid(y - 1 + t, ih) + 1
    dy = rem_euclid(y - 1 - t, ih) + 1
    lx = rem_euclid(x - 1 + t, iw) + 1
    rx = rem_euclid(x - 1 - t, iw) + 1

    MapSet.member?(up, uy * w + x) or
      MapSet.member?(down, dy * w + x) or
      MapSet.member?(left, y * w + lx) or
      MapSet.member?(right, y * w + rx)
  end

  defp parse(input) do
    lines = input |> String.trim() |> String.split("\n")
    h = length(lines)
    w = String.length(hd(lines))

    points =
      for {line, y} <- Enum.with_index(lines),
          {char, x} <- Enum.with_index(String.graphemes(line)),
          char in ["^", "v", "<", ">"] do
        {y * w + x, char}
      end

    up = filter_points(points, "^")
    down = filter_points(points, "v")
    left = filter_points(points, "<")
    right = filter_points(points, ">")

    {up, down, left, right, w, h}
  end

  defp filter_points(points, char) do
    points
    |> Enum.filter(fn {_, c} -> c == char end)
    |> Enum.map(fn {pos, _} -> pos end)
    |> MapSet.new()
  end

  defp lcm(a, b), do: div(abs(a * b), Integer.gcd(a, b))

  defp rem_euclid(a, b) do
    r = rem(a, b)
    if r < 0, do: r + b, else: r
  end
end
