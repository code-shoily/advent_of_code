defmodule AdventOfCode.Y2018.Day03 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2018/day/3
  """
  use AdventOfCode.Helpers.InputReader, year: 2018, day: 3

  def process(input) do
    input
    |> String.split("\n", trim: true)
    |> Stream.map(&parse_line/1)
    |> Stream.map(&sanitize/1)
    |> Enum.to_list()
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

  def populate_claims(input) do
    input
    |> process()
    |> Enum.reduce(%{}, &claim/2)
  end

  def run_1 do
    input!()
    |> populate_claims()
    |> Enum.filter(fn {_, v} -> length(v) > 1 end)
    |> length()
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

  def run_2 do
    input!()
    |> populate_claims()
    |> claim_world_view()
    |> Enum.filter(&elem(&1, 1))
    |> hd()
    |> elem(0)
  end

  def run, do: {run_1(), run_2()}
end
