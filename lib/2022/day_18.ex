defmodule AdventOfCode.Y2022.Day18 do
  @moduledoc """
  --- Day 18: Boiling Boulders ---
  Problem Link: https://adventofcode.com/2022/day/18
  Difficulty: xl
  Tags: geometry3d surface set grid traversal
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Traversal.Implicit

  def input, do: InputReader.read_from_file(2022, 18)

  @spec run(binary()) :: {number(), number()}
  def run(input \\ input()) do
    cubes = parse(input)
    {solve_part1(cubes), solve_part2(cubes)}
  end

  defp solve_part1(cubes) do
    Enum.reduce(cubes, 0, fn cube, acc ->
      neighbors(cube)
      |> Enum.count(fn n -> not MapSet.member?(cubes, n) end)
      |> Kernel.+(acc)
    end)
  end

  defp solve_part2(cubes) do
    {{min_x, max_x}, {min_y, max_y}, {min_z, max_z}} = bounds(cubes)

    range_x = (min_x - 1)..(max_x + 1)
    range_y = (min_y - 1)..(max_y + 1)
    range_z = (min_z - 1)..(max_z + 1)
    start_point = {min_x - 1, min_y - 1, min_z - 1}

    exterior_air =
      Implicit.implicit_fold(
        from: start_point,
        using: :breadth_first,
        successors_of: fn curr ->
          neighbors(curr)
          |> Enum.filter(fn {x, y, z} ->
            x in range_x and y in range_y and z in range_z and
              not MapSet.member?(cubes, {x, y, z})
          end)
        end,
        initial: MapSet.new(),
        with: fn acc, node, _meta ->
          {:continue, MapSet.put(acc, node)}
        end
      )

    Enum.reduce(cubes, 0, fn cube, acc ->
      neighbors(cube)
      |> Enum.count(fn n -> MapSet.member?(exterior_air, n) end)
      |> Kernel.+(acc)
    end)
  end

  defp neighbors({x, y, z}) do
    [
      {x + 1, y, z},
      {x - 1, y, z},
      {x, y + 1, z},
      {x, y - 1, z},
      {x, y, z + 1},
      {x, y, z - 1}
    ]
  end

  defp bounds(cubes) do
    Enum.reduce(cubes, {nil, nil, nil}, fn {x, y, z}, {rx, ry, rz} ->
      {update_range(rx, x), update_range(ry, y), update_range(rz, z)}
    end)
  end

  defp update_range(nil, v), do: {v, v}
  defp update_range({min, max}, v), do: {min(min, v), max(max, v)}

  defp parse(input) do
    input
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [x, y, z] = String.split(line, ",") |> Enum.map(&String.to_integer/1)
      {x, y, z}
    end)
    |> MapSet.new()
  end
end
