defmodule AdventOfCode.Y2023.Day21 do
  @moduledoc """
  --- Day 21: Step Counter ---
  Problem Link: https://adventofcode.com/2023/day/21
  Difficulty: l
  Tags: cycles graph grid math
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Traversal

  def input, do: InputReader.read_from_file(2023, 21)

  @spec run(binary()) :: {non_neg_integer(), integer()}
  def run(input \\ input()) do
    {grid, start, size} = parse_grid(input)

    {run_1(grid, start, 64), run_2(grid, start, size, 26_501_365)}
  end

  defp run_1(grid, start, steps) do
    distances =
      Traversal.implicit_fold(
        from: start,
        using: :breadth_first,
        initial: %{start => 0},
        successors_of: fn {x, y} ->
          for {dx, dy} <- [{0, 1}, {0, -1}, {1, 0}, {-1, 0}],
              nxt = {x + dx, y + dy},
              Map.get(grid, nxt) == :garden,
              do: nxt
        end,
        with: fn acc, pos, meta ->
          new_acc = Map.put(acc, pos, meta.depth)
          if meta.depth < steps, do: {:continue, new_acc}, else: {:stop, new_acc}
        end
      )

    distances
    |> Enum.count(fn {_, d} -> d <= steps and rem(d, 2) == rem(steps, 2) end)
  end

  defp run_2(grid, start, size, target_steps) do
    n = div(target_steps, size)
    remainder = rem(target_steps, size)

    f0 = count_reachable_infinite(grid, start, size, remainder)
    f1 = count_reachable_infinite(grid, start, size, remainder + size)
    f2 = count_reachable_infinite(grid, start, size, remainder + 2 * size)
    c = f0
    a = div(f2 - 2 * f1 + f0, 2)
    b = f1 - f0 - a

    a * n * n + b * n + c
  end

  defp count_reachable_infinite(grid, start, size, steps) do
    distances =
      Traversal.implicit_fold(
        from: start,
        using: :breadth_first,
        initial: %{start => 0},
        successors_of: fn {x, y} ->
          for {dx, dy} <- [{0, 1}, {0, -1}, {1, 0}, {-1, 0}],
              nx = x + dx,
              ny = y + dy,
              Map.get(grid, {Integer.mod(nx, size), Integer.mod(ny, size)}) == :garden,
              do: {nx, ny}
        end,
        with: fn acc, pos, meta ->
          new_acc = Map.put(acc, pos, meta.depth)
          if meta.depth < steps, do: {:continue, new_acc}, else: {:stop, new_acc}
        end
      )

    parity = rem(steps, 2)

    Enum.count(distances, fn {_, d} -> d <= steps and rem(d, 2) == parity end)
  end

  defp parse_grid(input) do
    lines = Transformers.lines(input)
    size = length(lines)

    grid =
      for {line, y} <- Enum.with_index(lines),
          {char, x} <- Enum.with_index(String.graphemes(line)),
          char != "#",
          into: %{} do
        {{x, y}, :garden}
      end

    # Find S
    [start] =
      for {line, y} <- Enum.with_index(lines),
          {char, x} <- Enum.with_index(String.graphemes(line)),
          char == "S",
          do: {x, y}

    {grid, start, size}
  end

  def parse(data \\ input()) do
    data |> Transformers.lines()
  end
end
