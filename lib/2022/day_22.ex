defmodule AdventOfCode.Y2022.Day22 do
  @moduledoc """
  --- Day 22: Monkey Map ---
  Problem Link: https://adventofcode.com/2022/day/22
  Difficulty: xxl
  Tags: half-done geometry3d walk3d
  """
  alias AdventOfCode.Algorithms.Grid
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 22, false)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1({instructions, path, rlims, clims, origin}) do
    instructions
    |> final_position(path, rlims, clims, origin)
    |> password()
  end

  defp run_2(_input) do
    {:todo, 2}
  end

  defp password({facing, {row, col}}),
    do: 1000 * (row + 1) + 4 * (col + 1) + Enum.find_index(~w/E S W N/, &(&1 == facing))

  defp final_position(instructions, path, rlims, clims, origin) do
    Enum.reduce(instructions, {"E", origin}, fn instruction, {facing, {x, y} = pos} ->
      case instruction do
        steps when is_integer(steps) ->
          walk(path, steps, facing, {x, y}, {rlims, clims})

        direction ->
          {change_facing(facing, direction), pos}
      end
    end)
  end

  defp walk(_, steps, facing, {x, y}, _) when steps <= 0, do: {facing, {x, y}}

  defp walk(path, steps, facing, {x, y}, {rlims, clims}) do
    {next_pos, wrapped} = get_coords(facing, {x, y}, {rlims, clims})

    case path[next_pos] do
      "." ->
        walk(path, steps - 1, facing, next_pos, {rlims, clims})

      "#" ->
        {facing, {x, y}}

      nil ->
        (path[wrapped] == "#" && {facing, {x, y}}) ||
          walk(path, steps - 1, facing, wrapped, {rlims, clims})
    end
  end

  defp get_coords(facing, {x, y}, {rlims, clims}) do
    {min_col, max_col} = clims[y]
    {min_row, max_row} = rlims[x]

    case facing do
      "N" -> {{x - 1, y}, {max_col, y}}
      "S" -> {{x + 1, y}, {min_col, y}}
      "E" -> {{x, y + 1}, {x, min_row}}
      "W" -> {{x, y - 1}, {x, max_row}}
    end
  end

  defp change_facing("N", dir), do: (dir == "L" && "W") || "E"
  defp change_facing("S", dir), do: (dir == "L" && "E") || "W"
  defp change_facing("E", dir), do: (dir == "L" && "N") || "S"
  defp change_facing("W", dir), do: (dir == "L" && "S") || "N"

  def parse(data \\ input()) do
    {instructions, path} = List.pop_at(Transformers.lines(data), -1)
    path = parse_path(path)

    {parse_instructions(instructions), path, limits(path, 0, 1), limits(path, 1, 0),
     Enum.min(Map.keys(path))}
  end

  defp parse_path(path) do
    path
    |> Enum.map(&String.graphemes/1)
    |> Grid.grid2d()
    |> Map.reject(fn {_, v} -> v == " " end)
  end

  defp parse_instructions(instructions) do
    instructions
    |> String.graphemes()
    |> Enum.chunk_by(fn i -> i in ~w/L R/ end)
    |> Enum.map(fn line ->
      case Integer.parse(line = Enum.join(line)) do
        {num, ""} -> num
        :error -> line
      end
    end)
  end

  defp limits(path, group, value) do
    path
    |> Map.keys()
    |> Enum.group_by(&elem(&1, group), &elem(&1, value))
    |> Map.new(fn {k, v} -> {k, Enum.min_max(v)} end)
  end
end
