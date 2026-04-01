defmodule AdventOfCode.Y2021.Day25 do
  @moduledoc """
  --- Day 25: Sea Cucumber ---
  Problem Link: https://adventofcode.com/2021/day/25
  Difficulty: m
  Tags: grid set
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Builder.Grid

  def input, do: InputReader.read_from_file(2021, 25)

  def run(input \\ input()) do
    {east, south, dims} = parse(input)
    {part_1(east, south, dims, 1), "🎉"}
  end

  def parse(data) do
    lines = Transformers.lines(data)
    rows = length(lines)
    cols = String.length(hd(lines))

    {east, south} =
      for {line, r} <- Enum.with_index(lines),
          {char, c} <- Enum.with_index(String.graphemes(line)),
          char in [">", "v"],
          reduce: {MapSet.new(), MapSet.new()} do
        {e, s} ->
          id = Grid.coord_to_id(r, c, cols)
          if char == ">", do: {MapSet.put(e, id), s}, else: {e, MapSet.put(s, id)}
      end

    {east, south, {rows, cols}}
  end

  defp part_1(east, south, dims, step) do
    new_east = move_east(east, south, dims)
    new_south = move_south(new_east, south, dims)

    if new_east == east and new_south == south do
      step
    else
      part_1(new_east, new_south, dims, step + 1)
    end
  end

  defp move_east(east, south, {_rows, cols}) do
    Enum.reduce(east, MapSet.new(), fn cid, acc ->
      target_id = if rem(cid + 1, cols) == 0, do: cid - (cols - 1), else: cid + 1

      if not MapSet.member?(east, target_id) and not MapSet.member?(south, target_id) do
        MapSet.put(acc, target_id)
      else
        MapSet.put(acc, cid)
      end
    end)
  end

  defp move_south(east, south, {rows, cols}) do
    total = rows * cols

    Enum.reduce(south, MapSet.new(), fn cid, acc ->
      target_id = if cid + cols >= total, do: cid + cols - total, else: cid + cols

      if not MapSet.member?(east, target_id) and not MapSet.member?(south, target_id) do
        MapSet.put(acc, target_id)
      else
        MapSet.put(acc, cid)
      end
    end)
  end
end
