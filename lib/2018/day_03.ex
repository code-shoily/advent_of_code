defmodule AdventOfCode.Y2018.Day03 do
  @moduledoc """
  --- Day 3: No Matter How You Slice It ---
  Problem Link: https://adventofcode.com/2018/day/3
  Difficulty: m
  Tags: grid set not-fast-enough
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2018, 3)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  def run_1(input) do
    input
    |> Enum.filter(fn {_, v} -> length(v) > 1 end)
    |> length()
  end

  def run_2(input) do
    input
    |> claim_world_view()
    |> Enum.filter(&elem(&1, 1))
    |> List.first()
    |> elem(0)
  end

  def parse(input \\ input()) do
    input
    |> Transformers.lines()
    |> Stream.map(&parse_line/1)
    |> Stream.map(&sanitize/1)
    |> Enum.to_list()
    |> Enum.reduce(%{}, &claim/2)
  end

  @regex ~r/#(?<id>\d+)\s@\s(?<x>\d+),(?<y>\d+):\s(?<w>\d+)x(?<h>\d+)/
  defp parse_line(line) do
    @regex
    |> Regex.named_captures(line)
    |> Enum.map(fn {k, v} -> {String.to_atom(k), String.to_integer(v)} end)
    |> Enum.into(%{})
  end

  defp sanitize(%{id: id, x: x, y: y, w: w, h: h}) do
    %{
      id: id,
      origin: {x, y},
      dimension: {w, h}
    }
  end

  def claim(%{id: id, origin: {x, y}, dimension: {w, h}}, world) do
    for i <- x..(x + w - 1) do
      for j <- y..(y + h - 1) do
        {i, j}
      end
    end
    |> Enum.flat_map(& &1)
    |> Enum.reduce(world, fn coord, new_world ->
      Map.update(new_world, coord, [id], fn ids -> [id | ids] end)
    end)
  end

  def claim_world_view(claims) do
    claims
    |> Enum.reduce(%{}, fn {_, ids}, world ->
      case ids do
        [id] -> Map.put_new(world, id, true)
        many -> Map.merge(world, Map.new(many, &{&1, false}))
      end
    end)
  end
end
