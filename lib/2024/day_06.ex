defmodule AdventOfCode.Y2024.Day06 do
  @moduledoc """
  --- Day 6: Guard Gallivant ---
  Problem Link: https://adventofcode.com/2024/day/6
  Difficulty: m
  Tags: grid simulation cycle-detection slow
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2024, 6)

  # Directions: up, right, down, left
  @dx {0, 1, 0, -1}
  @dy {-1, 0, 1, 0}

  def run(input \\ input()) do
    {grid, rows, cols, sx, sy} = parse(input)
    {run_1(grid, rows, cols, sx, sy), run_2(grid, rows, cols, sx, sy)}
  end

  # Part 1: Simulate guard path
  defp run_1(grid, rows, cols, sx, sy) do
    {pos_map, _} = simulate(grid, rows, cols, sx, sy, 0, %{}, %{})
    map_size(pos_map)
  end

  # Simulate guard movement - returns {position_map, :exit | :loop}
  defp simulate(grid, rows, cols, x, y, dir, pos_map, state_map) do
    # Encode state as integer for fast lookup
    state_key = (x * rows + y) * 4 + dir

    if Map.has_key?(state_map, state_key) do
      {pos_map, :loop}
    else
      new_state = Map.put(state_map, state_key, true)
      new_pos = Map.put(pos_map, {x, y}, true)

      nx = x + elem(@dx, dir)
      ny = y + elem(@dy, dir)

      cond do
        nx < 0 or nx >= cols or ny < 0 or ny >= rows ->
          {new_pos, :exit}

        elem(grid, ny * cols + nx) == ?# ->
          simulate(grid, rows, cols, x, y, rem(dir + 1, 4), new_pos, new_state)

        true ->
          simulate(grid, rows, cols, nx, ny, dir, new_pos, new_state)
      end
    end
  end

  # Part 2: Count positions where adding an obstacle creates a loop
  defp run_2(grid, rows, cols, sx, sy) do
    {orig_pos, _} = simulate(grid, rows, cols, sx, sy, 0, %{}, %{})

    orig_pos
    |> Map.keys()
    |> Enum.reject(fn {px, py} -> px == sx and py == sy end)
    |> Task.async_stream(
      fn {ox, oy} ->
        new_grid = put_elem(grid, oy * cols + ox, ?#)

        case simulate(new_grid, rows, cols, sx, sy, 0, %{}, %{}) do
          {_, :loop} -> 1
          _ -> 0
        end
      end,
      ordered: false,
      max_concurrency: System.schedulers_online()
    )
    |> Enum.reduce(0, fn {:ok, count}, acc -> acc + count end)
  end

  # Parse into flat tuple for O(1) access
  defp parse(input) do
    lines = Transformers.lines(input)
    rows = length(lines)
    cols = String.length(hd(lines))

    {grid_list_rev, sx, sy} =
      lines
      |> Enum.with_index()
      |> Enum.reduce({[], nil, nil}, fn {line, y}, {acc, sx, sy} ->
        chars = String.to_charlist(line)

        {new_sx, new_sy} =
          case Enum.find_index(chars, &(&1 == ?^)) do
            nil -> {sx, sy}
            x -> {x, y}
          end

        normalized = Enum.map(chars, fn c -> if c == ?^, do: ?., else: c end)
        {[normalized | acc], new_sx, new_sy}
      end)

    grid_tuple =
      grid_list_rev
      |> Enum.reverse()
      |> List.flatten()
      |> List.to_tuple()

    {grid_tuple, rows, cols, sx, sy}
  end
end
