defmodule AdventOfCode.Y2015.Day18 do
  @moduledoc """
  --- Day 18: Like a GIF For Your Yard ---
  Problem Link: https://adventofcode.com/2015/day/18
  Difficulty: m
  Tags: grid map simulation
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @rows 100
  @cols 100
  @corners [{0, 0}, {0, 99}, {99, 0}, {99, 99}]

  def input, do: InputReader.read_from_file(2015, 18)

  def run(input \\ input()) do
    on_set = parse(input)

    task_1 = Task.async(fn -> solve(on_set, false) end)
    task_2 = Task.async(fn -> solve(on_set, true) end)

    {Task.await(task_1), Task.await(task_2)}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.with_index()
    |> Enum.reduce(MapSet.new(), fn {line, r}, acc ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {char, c}, inner_acc ->
        if char == "#", do: MapSet.put(inner_acc, {r, c}), else: inner_acc
      end)
    end)
  end

  defp solve(on_set, corners_sticky?) do
    on_set = if corners_sticky?, do: MapSet.union(on_set, MapSet.new(@corners)), else: on_set

    1..100
    |> Enum.reduce(on_set, fn _, current_on ->
      # Step 1: Count neighbors for all "interesting" cells
      neighbor_counts =
        Enum.reduce(current_on, %{}, fn {r, c}, counts ->
          Enum.reduce(neighbors(r, c), counts, fn pos, acc ->
            Map.update(acc, pos, 1, &(&1 + 1))
          end)
        end)

      # Step 2: Apply GOL rules
      new_on =
        Enum.reduce(neighbor_counts, MapSet.new(), fn {pos, count}, acc ->
          cond do
            count == 3 -> MapSet.put(acc, pos)
            count == 2 and MapSet.member?(current_on, pos) -> MapSet.put(acc, pos)
            true -> acc
          end
        end)

      if corners_sticky?, do: MapSet.union(new_on, MapSet.new(@corners)), else: new_on
    end)
    |> MapSet.size()
  end

  defp neighbors(r, c) do
    for dr <- -1..1,
        dc <- -1..1,
        not (dr == 0 and dc == 0),
        nr = r + dr,
        nc = c + dc,
        nr >= 0 and nr < @rows and nc >= 0 and nc < @cols do
      {nr, nc}
    end
  end
end
