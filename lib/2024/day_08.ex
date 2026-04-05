defmodule AdventOfCode.Y2024.Day08 do
  @moduledoc """
  --- Day 8: Resonant Collinearity ---
  Problem Link: https://adventofcode.com/2024/day/8
  Difficulty: s
  Tags: grid enumeration coordinate-geometry
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2024, 8)

  def run(input \\ input()) do
    {width, height, antennas} = parse(input)

    p1 = solve(width, height, antennas, :part1)
    p2 = solve(width, height, antennas, :part2)

    {p1, p2}
  end

  defp solve(width, height, antennas, part) do
    for {_freq, coords} <- antennas,
        a1 <- coords,
        a2 <- coords,
        a1 != a2,
        reduce: MapSet.new() do
      acc ->
        generate_antinodes(a1, a2, width, height, part)
        |> Enum.reduce(acc, &MapSet.put(&2, &1))
    end
    |> MapSet.size()
  end

  defp generate_antinodes({x1, y1}, {x2, y2}, w, h, :part1) do
    dx = x2 - x1
    dy = y2 - y1

    [{x1 - dx, y1 - dy}, {x2 + dx, y2 + dy}]
    |> Enum.filter(&in_bounds?(&1, w, h))
  end

  defp generate_antinodes({x1, y1}, {x2, y2}, w, h, :part2) do
    dx = x2 - x1
    dy = y2 - y1
    g = Integer.gcd(abs(dx), abs(dy))
    step_x = div(dx, g)
    step_y = div(dy, g)

    collect_line({x1, y1}, {step_x, step_y}, w, h) ++
      collect_line({x1, y1}, {-step_x, -step_y}, w, h)
  end

  defp collect_line({x, y}, {dx, dy}, w, h) do
    if in_bounds?({x, y}, w, h) do
      [{x, y} | collect_line({x + dx, y + dy}, {dx, dy}, w, h)]
    else
      []
    end
  end

  defp in_bounds?({x, y}, w, h), do: x >= 0 and x < w and y >= 0 and y < h

  def parse(data \\ input()) do
    lines = Transformers.lines(data)
    height = length(lines)
    width = lines |> List.first() |> String.length()

    antennas =
      lines
      |> Enum.with_index()
      |> Enum.reduce(%{}, fn {line, y}, acc ->
        line
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.reduce(acc, fn
          {".", _}, acc -> acc
          {char, x}, acc ->
            Map.update(acc, char, [{x, y}], &[{x, y} | &1])
        end)
      end)

    {width, height, antennas}
  end
end
