defmodule AdventOfCode.Y2019.Day03 do
  @moduledoc """
  --- Day 3: Crossed Wires ---
  Problem Link: https://adventofcode.com/2019/day/3
  Difficulty: xs
  Tags: grid set
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  alias Yog.Builder.Grid
  import AdventOfCode.Algorithms.Geometry, only: [manhattan_distance: 2]

  @cols 30_000
  @offset 15_000
  @origin_id Grid.coord_to_id(@offset, @offset, @cols)

  def input, do: InputReader.read_from_file(2019, 3)

  def run(input \\ input()) do
    input = parse(input)
    {run_1(input), run_2(input)}
  end

  def run_1(input) do
    [wire1, wire2] = Enum.map(input, &trace_path/1)

    wire1
    |> Map.keys()
    |> MapSet.new()
    |> MapSet.intersection(MapSet.new(Map.keys(wire2)))
    |> MapSet.delete(@origin_id)
    |> Enum.map(fn id ->
      {x, y} = Grid.id_to_coord(id, @cols)
      manhattan_distance({@offset, @offset}, {x, y})
    end)
    |> Enum.min()
  end

  def run_2(input) do
    [wire1, wire2] = Enum.map(input, &trace_path/1)

    wire1
    |> Map.keys()
    |> Enum.reduce(:infinity, fn id, min_steps ->
      if id != @origin_id and Map.has_key?(wire2, id) do
        min(min_steps, wire1[id] + wire2[id])
      else
        min_steps
      end
    end)
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      line
      |> String.split(",")
      |> Enum.map(&parse_instruction/1)
    end)
  end

  defp parse_instruction(instruction) do
    instruction
    |> String.split_at(1)
    |> then(fn {dir, val} -> {dir, String.to_integer(val)} end)
  end

  defp trace_path(instructions) do
    {_, _, _, path} =
      Enum.reduce(instructions, {@offset, @offset, 0, %{@origin_id => 0}}, fn {dir, dist},
                                                                              {r, c, steps, acc} ->
        {dr, dc} = delta(dir)

        Enum.reduce(1..dist, {r, c, steps, acc}, fn _, {curr_r, curr_c, curr_steps, inner_acc} ->
          nr = curr_r + dr
          nc = curr_c + dc
          ns = curr_steps + 1
          id = Grid.coord_to_id(nr, nc, @cols)
          {nr, nc, ns, Map.put_new(inner_acc, id, ns)}
        end)
      end)

    path
  end

  defp delta("U"), do: {-1, 0}
  defp delta("D"), do: {1, 0}
  defp delta("L"), do: {0, -1}
  defp delta("R"), do: {0, 1}
end
