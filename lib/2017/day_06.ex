defmodule AdventOfCode.Y2017.Day06 do
  @moduledoc """
  --- Day 6: Memory Reallocation ---
  Problem Link: https://adventofcode.com/2017/day/6
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 6)

  def run(input \\ input()) do
    blocks = parse(input)
    {visited, blocks} = distribute_largest(blocks, %MapSet{})
    {second_visit, _} = distribute_largest(blocks, %MapSet{})

    {Enum.count(visited) + 1, Enum.count(second_visit)}
  end

  def distribute_largest(blocks, visited) do
    blocks = do_distribute_largest(blocks)

    case MapSet.member?(visited, blocks) do
      true -> {visited, blocks}
      _ -> distribute_largest(blocks, MapSet.put(visited, blocks))
    end
  end

  def do_distribute_largest(blocks) do
    {idx, value} = Enum.max_by(blocks, fn {_, v} -> v end)

    Enum.reduce(0..(value - 1), Map.put(blocks, idx, 0), fn i, acc ->
      Map.update(acc, rem(idx + i + 1, Enum.count(blocks)), nil, &(&1 + 1))
    end)
  end

  def parse(data \\ input()) do
    data
    |> Transformers.words("\t")
    |> Enum.map(&String.to_integer/1)
    |> Enum.with_index()
    |> Map.new(fn {v, k} -> {k, v} end)
  end
end
