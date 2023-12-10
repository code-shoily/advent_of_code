defmodule AdventOfCode.Y2017.Day11 do
  @moduledoc """
  --- Day 11: Hex Ed ---
  Problem Link: https://adventofcode.com/2017/day/11
  Difficulty: l
  Tags: hexagon

  Help: https://www.redblobgames.com/grids/hexagons/
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 11)

  def run(input \\ input()) do
    dirs = parse(input)
    {distance, farthest, _} = journey(dirs)

    {distance, farthest}
  end

  def parse(data \\ input()) do
    data
    |> Transformers.words(",")
    |> Enum.map(&String.to_existing_atom/1)
  end

  defp journey(dirs) do
    Enum.reduce(dirs, {0, 0, {0, 0, 0}}, fn dir, {_, farthest, position} ->
      new_position = walk(dir, position)
      distance = get_distance(new_position)
      farthest = max(distance, farthest)

      {distance, farthest, new_position}
    end)
  end

  defp walk(:n, {x, y, z}), do: {x, y + 1, z - 1}
  defp walk(:ne, {x, y, z}), do: {x + 1, y, z - 1}
  defp walk(:nw, {x, y, z}), do: {x - 1, y + 1, z}
  defp walk(:s, {x, y, z}), do: {x, y - 1, z + 1}
  defp walk(:se, {x, y, z}), do: {x + 1, y - 1, z}
  defp walk(:sw, {x, y, z}), do: {x - 1, y, z + 1}

  defp get_distance(position), do: get_distance({0, 0, 0}, position)

  defp get_distance({x1, y1, z1}, {x2, y2, z2}) do
    Enum.max([abs(x2 - x1), abs(y2 - y1), abs(z2 - z1)])
  end
end
