defmodule AdventOfCode.Y2020.Day20 do
  @moduledoc """
  --- Day 20: Jurassic Jigsaw ---
  Problem Link: https://adventofcode.com/2020/day/20
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 20)

  def run(input \\ input()) do
    input = parse(input)
    {product(corners(input)), {:todo, 2}}
  end

  def parse(input), do: input |> String.split(~r{(\r\n\r\n|\r\r|\n\n)}) |> Map.new(&parse_tiles/1)

  defp parse_tiles(tile) do
    [id | tiles] = tile |> Transformers.lines()
    {extract_id(id), edges(Enum.map(tiles, &String.graphemes/1))}
  end

  @regex ~r/Tile (?<id>\d+):/
  defp extract_id(id), do: Regex.named_captures(@regex, id)["id"]
  defp product(ids), do: Enum.reduce(Enum.map(ids, &String.to_integer/1), &(&1 * &2))

  defp edges(tiles) do
    transposed = Transformers.transpose(tiles)

    Enum.map(
      [hd(tiles), List.last(tiles), hd(transposed), List.last(transposed)],
      fn edge -> MapSet.new([Enum.join(edge), Enum.join(Enum.reverse(edge))]) end
    )
  end

  defp corners(data), do: Enum.reject(find_unique(all_edges(data), data), &is_nil/1)

  defp all_edges(data) do
    data
    |> Map.values()
    |> Enum.flat_map(& &1)
    |> Enum.frequencies()
    |> Map.filter(fn {_, c} -> c == 1 end)
    |> Map.keys()
    |> MapSet.new()
  end

  defp find_unique(edges, tiles) do
    tiles
    |> Enum.map(fn {k, v} ->
      case Enum.count(MapSet.intersection(MapSet.new(v), edges)) do
        2 -> k
        _ -> nil
      end
    end)
  end
end
