defmodule AdventOfCode.Y2018.Day08 do
  @moduledoc """
  --- Day 8: Memory Maneuver ---
  Problem Link: https://adventofcode.com/2018/day/8
  """
  use AdventOfCode.Helpers.InputReader, year: 2018, day: 8

  def run_1, do: input!() |> parse() |> metadata_sum()
  def run_2, do: {:not_implemented, 2}

  def parse(data) do
    data
    |> String.split(" ", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  # --- <Solution Functions> ---
  def metadata_sum(lst), do: walk(lst, [], [], []) |> Enum.flat_map(& &1) |> Enum.sum()
  def walk([], _, _, metadata), do: metadata

  def walk([node_n, meta_n | rest], [], meta_ns, []),
    do: walk(rest, [node_n], [meta_n | meta_ns], [])

  def walk(rest, [0 | nodes], [meta_last | meta_ns], metadata) do
    {metas, next} = Enum.split(rest, meta_last)
    walk(next, nodes, meta_ns, [metas | metadata])
  end

  def walk([node_n, meta_n | rest], [node_last | nodes], meta_ns, metadata),
    do: walk(rest, [node_n, node_last - 1 | nodes], [meta_n | meta_ns], metadata)

  def values(lst), do: values(lst, [], [], [])
  def values([], _, _, metadata), do: metadata

  def values([node_n, meta_n | rest], [], meta_ns, []) do
    values(rest, [node_n], [meta_n | meta_ns], [])
  end

  def values(rest, [0 | nodes], [meta_last | meta_ns], metadata) do
    {metas, next} = Enum.split(rest, meta_last)
    values(next, nodes, meta_ns, [metas | metadata])
  end

  def values([node_n, meta_n | rest], [node_last | nodes], meta_ns, metadata) do
    values(rest, [node_n, node_last - 1 | nodes], [meta_n | meta_ns], metadata)
  end

  # --- </Solution Functions> ---
end
