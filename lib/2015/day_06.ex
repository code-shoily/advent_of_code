defmodule AdventOfCode.Y2015.Day06 do
  @moduledoc """
  --- Day 6: Probably a Fire Hazard ---
  Problem Link: https://adventofcode.com/2015/day/6
  Difficulty: m
  Tags: grid vector reduction slow
  """
  alias Aja.Vector
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2015, 6)

  def run(input \\ input()) do
    input = Enum.map(Transformers.lines(input), &parse_input/1)
    grid = make_grid(1000)

    task_1 = Task.async(fn -> brightness(Enum.reduce(input, grid, &apply_1/2)) end)
    task_2 = Task.async(fn -> brightness(Enum.reduce(input, grid, &apply_2/2)) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def apply_1(line, grid),
    do: apply(line, grid, fn _ -> 1 end, fn _ -> 0 end, fn v -> (v == 0 && 1) || 0 end)

  def apply_2(line, grid), do: apply(line, grid, &(&1 + 1), &max(0, &1 - 1), &(&1 + 2))

  defp apply({cmd, xs, ys}, grid, on, off, toggle) do
    Vector.foldl(coords(xs, ys), grid, fn [x, y], acc ->
      case cmd do
        :turn_on -> Vector.update_at(acc, x, &Vector.update_at(&1, y, on))
        :turn_off -> Vector.update_at(acc, x, &Vector.update_at(&1, y, off))
        :toggle -> Vector.update_at(acc, x, &Vector.update_at(&1, y, toggle))
      end
    end)
  end

  defp make_grid(dim), do: Vector.duplicate(Vector.duplicate(0, dim), dim)
  defp as_atom("turn on"), do: :turn_on
  defp as_atom("turn off"), do: :turn_off
  defp as_atom("toggle"), do: :toggle
  defp brightness(grid), do: Aja.Enum.reduce(grid, 0, &(&2 + Aja.Enum.sum(&1)))
  defp coords(xs, ys), do: Vector.new(for x <- xs, y <- ys, do: [x, y])

  @regex ~r"(?<cmd>toggle|turn\son|turn\soff)\s(?<x1>\d+),(?<y1>\d+)\s\S+\s(?<x2>\d+),(?<y2>\d+)"
  defp parse_input(line), do: format(Regex.named_captures(@regex, line))

  defp format(%{"cmd" => cmd, "x1" => x1, "x2" => x2, "y1" => y1, "y2" => y2}) do
    {as_atom(cmd), String.to_integer(x1)..String.to_integer(x2),
     String.to_integer(y1)..String.to_integer(y2)}
  end
end
