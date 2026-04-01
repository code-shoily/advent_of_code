defmodule AdventOfCode.Y2022.Day22 do
  @moduledoc """
  --- Day 22: Monkey Map ---
  Problem Link: https://adventofcode.com/2022/day/22
  Difficulty: xl
  Tags: geometry3d walk3d
  """
  alias AdventOfCode.Algorithms.Grid
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 22, false)

  def run(input \\ input()) do
    data = parse(input)
    {run_1(data), run_2(data)}
  end

  defp run_1({instructions, path, rlims, clims, origin}) do
    instructions
    |> final_position("E", origin, path, {:flat, rlims, clims})
    |> password()
  end

  defp run_2({instructions, path, _rlims, _clims, origin}) do
    instructions
    |> final_position("E", origin, path, :cube)
    |> password()
  end

  defp password({facing, {row, col}}),
    do: 1000 * (row + 1) + 4 * (col + 1) + Enum.find_index(~w/E S W N/, &(&1 == facing))

  defp final_position(instructions, start_facing, start_pos, path, mode) do
    Enum.reduce(instructions, {start_facing, start_pos}, fn instruction, {facing, pos} ->
      case instruction do
        steps when is_integer(steps) -> walk(path, steps, facing, pos, mode)
        turn -> {change_facing(facing, turn), pos}
      end
    end)
  end

  defp walk(_, 0, facing, pos, _), do: {facing, pos}

  defp walk(path, steps, facing, {_r, _c} = pos, mode) do
    {n_facing, {_nr, _nc} = n_pos} = step(facing, pos, mode)

    case Map.get(path, n_pos) do
      "." -> walk(path, steps - 1, n_facing, n_pos, mode)
      "#" -> {facing, pos}
      nil -> {facing, pos}
    end
  end

  defp step(facing, {r, c}, {:flat, rlims, clims}) do
    {dr, dc} = delta(facing)
    nr = r + dr
    nc = c + dc

    if Map.has_key?(rlims, nr) and nc >= elem(rlims[nr], 0) and nc <= elem(rlims[nr], 1) and
         Map.has_key?(clims, nc) and nr >= elem(clims[nc], 0) and nr <= elem(clims[nc], 1) do
      {facing, {nr, nc}}
    else
      # Wrap flat
      case facing do
        "N" -> {facing, {elem(clims[c], 1), c}}
        "S" -> {facing, {elem(clims[c], 0), c}}
        "E" -> {facing, {r, elem(rlims[r], 0)}}
        "W" -> {facing, {r, elem(rlims[r], 1)}}
      end
    end
  end

  defp step(facing, {r, c}, :cube) do
    {dr, dc} = delta(facing)
    nr = r + dr
    nc = c + dc

    # Hardcoded transitions for the standard 50x50 layout
    # Faces:
    # A: r 0-49, c 50-99
    # B: r 0-49, c 100-149
    # C: r 50-99, c 50-99
    # D: r 100-149, c 0-49
    # E: r 100-149, c 50-99
    # F: r 150-199, c 0-49

    cond do
      facing == "N" and r == 0 and c in 50..99 -> {"E", {150 + (c - 50), 0}}
      facing == "N" and r == 0 and c in 100..149 -> {"N", {199, c - 100}}
      facing == "N" and r == 100 and c in 0..49 -> {"E", {50 + c, 50}}
      facing == "S" and r == 49 and c in 100..149 -> {"W", {50 + (c - 100), 99}}
      facing == "S" and r == 149 and c in 50..99 -> {"W", {150 + (c - 50), 49}}
      facing == "S" and r == 199 and c in 0..49 -> {"S", {0, 100 + c}}
      facing == "E" and c == 149 and r in 0..49 -> {"W", {149 - r, 99}}
      facing == "E" and c == 99 and r in 50..99 -> {"N", {49, 100 + (r - 50)}}
      facing == "E" and c == 99 and r in 100..149 -> {"W", {49 - (r - 100), 149}}
      facing == "E" and c == 49 and r in 150..199 -> {"N", {149, 50 + (r - 150)}}
      facing == "W" and c == 50 and r in 0..49 -> {"E", {149 - r, 0}}
      facing == "W" and c == 50 and r in 50..99 -> {"S", {100, r - 50}}
      facing == "W" and c == 0 and r in 100..149 -> {"E", {49 - (r - 100), 50}}
      facing == "W" and c == 0 and r in 150..199 -> {"S", {0, 50 + (r - 150)}}
      true -> {facing, {nr, nc}}
    end
  end

  defp delta("N"), do: {-1, 0}
  defp delta("S"), do: {1, 0}
  defp delta("E"), do: {0, 1}
  defp delta("W"), do: {0, -1}

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
