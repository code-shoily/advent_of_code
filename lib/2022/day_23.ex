defmodule AdventOfCode.Y2022.Day23 do
  @moduledoc """
  --- Day 23: Unstable Diffusion ---
  Problem Link: https://adventofcode.com/2022/day/23
  Difficulty: m
  Tags: grid walk
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Grid

  @offset 1000
  @cols 3000

  def input, do: InputReader.read_from_file(2022, 23)

  def run(input \\ input()) do
    elves = parse(input)
    {solve_1(elves), solve_2(elves)}
  end

  def parse(input) do
    input
    |> Transformers.lines()
    |> Enum.with_index()
    |> Enum.reduce(MapSet.new(), fn {line, r}, acc ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.reduce(acc, fn {cell, c}, acc_inner ->
        if cell == "#" do
          MapSet.put(acc_inner, Grid.coord_to_id(r + @offset, c + @offset, @cols))
        else
          acc_inner
        end
      end)
    end)
  end

  @neighbor_deltas for dr <- -1..1, dc <- -1..1, not (dr == 0 and dc == 0), do: dr * @cols + dc

  @checks [
    {[:nw, :n, :ne], :n},
    {[:sw, :s, :se], :s},
    {[:nw, :w, :sw], :w},
    {[:ne, :e, :se], :e}
  ]

  @deltas %{
    nw: -@cols - 1,
    n: -@cols,
    ne: -@cols + 1,
    w: -1,
    e: 1,
    sw: @cols - 1,
    s: @cols,
    se: @cols + 1
  }

  def solve_1(elves) do
    elves = loop(elves, @checks, 1, 10)
    {{min_r, max_r}, {min_c, max_c}} = bounding_box(elves)
    (max_r - min_r + 1) * (max_c - min_c + 1) - MapSet.size(elves)
  end

  def solve_2(elves) do
    loop(elves, @checks, 1, :infinity)
  end

  defp loop(elves, _checks, count, last_round) when count > last_round and is_integer(last_round),
    do: elves

  defp loop(elves, checks, count, last_round) do
    proposals_map =
      elves
      |> Enum.reduce(%{}, fn elf, acc ->
        case propose(elf, elves, checks) do
          nil -> acc
          target -> Map.update(acc, target, [elf], &[elf | &1])
        end
      end)

    actual_moves =
      Enum.reduce(proposals_map, %{}, fn {target, froms}, acc ->
        case froms do
          [from] -> Map.put(acc, from, target)
          _ -> acc
        end
      end)

    if map_size(actual_moves) == 0 do
      if last_round == :infinity, do: count, else: elves
    else
      new_elves =
        elves
        |> Enum.map(fn elf -> Map.get(actual_moves, elf, elf) end)
        |> MapSet.new()

      [h | t] = checks
      loop(new_elves, t ++ [h], count + 1, last_round)
    end
  end

  defp propose(elf, elves, checks) do
    has_neighbor = Enum.any?(@neighbor_deltas, fn delta -> MapSet.member?(elves, elf + delta) end)

    if has_neighbor do
      Enum.find_value(checks, fn {dirs, move_dir} ->
        if Enum.all?(dirs, fn dir -> !MapSet.member?(elves, elf + @deltas[dir]) end) do
          elf + @deltas[move_dir]
        else
          nil
        end
      end)
    else
      nil
    end
  end

  defp bounding_box(elves) do
    coords = elves |> Enum.map(&Grid.id_to_coord(&1, @cols))
    {min_r, max_r} = coords |> Enum.map(&elem(&1, 0)) |> Enum.min_max()
    {min_c, max_c} = coords |> Enum.map(&elem(&1, 1)) |> Enum.min_max()
    {{min_r, max_r}, {min_c, max_c}}
  end
end
