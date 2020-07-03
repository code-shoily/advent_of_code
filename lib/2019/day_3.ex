defmodule AdventOfCode.Y2019.Day3 do
  @moduledoc """
  Problem description: Problem description: https://adventofcode.com/2019/day/3
  """
  use AdventOfCode.Data.InputReader, year: 2019, day: 3

  @origin {0, 0}

  def process do
    input!()
    |> String.split("\n")
    |> Enum.map(fn line ->
      line
      |> String.split(",")
      |> Enum.map(fn instruction ->
        instruction
        |> String.split_at(1)
        |> (fn {dir, val} -> {dir, String.to_integer(val)} end).()
      end)
    end)
  end

  def run, do: {run_1(), run_2()}

  def run_1 do
    process()
    |> Enum.map(&move(&1, @origin, []))
    |> nearest_intersection()
  end

  defp move([instruction | []], current, results) do
    Enum.concat(
      results,
      points_between(current, compute_next(current, instruction))
    )
  end

  defp move([instruction | rest], current, results) do
    next = compute_next(current, instruction)

    move(
      rest,
      next,
      Enum.concat(results, points_between(current, next))
    )
  end

  defp compute_next({x, y}, {"L", value}), do: {x - value, y}
  defp compute_next({x, y}, {"R", value}), do: {x + value, y}
  defp compute_next({x, y}, {"U", value}), do: {x, y + value}
  defp compute_next({x, y}, {"D", value}), do: {x, y - value}

  defp points_between({x1, y1}, {x2, y2}) do
    for x <- x1..x2, y <- y1..y2, do: {x, y}
  end

  defp manhattan({x, y}), do: abs(x) + abs(y)

  defp nearest_intersection([first, second]) do
    first
    |> MapSet.new()
    |> MapSet.intersection(MapSet.new(second))
    |> MapSet.delete(@origin)
    |> Enum.min_by(&manhattan/1)
    |> manhattan()
  end

  def run_2 do
    process()
    |> Enum.map(fn wire ->
      wire
      |> move(@origin, [])
      |> Enum.dedup()
      |> calculate_steps(0, %{})
    end)
    |> (fn [a, b] ->
          Map.merge(a, b, fn
            _, 0, 0 -> :discard
            _, a, b -> [a, b]
          end)
        end).()
    |> Enum.filter(fn {_, v} -> is_list(v) end)
    |> Enum.map(fn {_, steps} -> steps end)
    |> Enum.min_by(fn [first, second] -> first + second end)
    |> Enum.sum()
  end

  defp calculate_steps([p], step, results), do: Map.put_new(results, p, step)

  defp calculate_steps([first | rest], 0, %{}) do
    calculate_steps(rest, 1, %{first => 0})
  end

  defp calculate_steps([first | rest], step, results) do
    calculate_steps(rest, step + 1, Map.put_new(results, first, step))
  end
end
