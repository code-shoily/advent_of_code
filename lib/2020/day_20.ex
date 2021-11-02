defmodule AdventOfCode.Y2020.Day20 do
  @moduledoc """
  --- Day 20: Jurassic Jigsaw ---
  Problem Link: https://adventofcode.com/2020/day/20
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 20

  import AdventOfCode.Helpers.Transformers

  def run_1, do: input!() |> parse() |> corners() |> product()
  def run_2, do: {:not_implemented, 2}
  def parse(input), do: String.split(input, "\n\n") |> Enum.map(&parse_tiles/1) |> Map.new()

  defp parse_tiles(tile) do
    [id | tiles] = String.split(tile, "\n")
    {extract_id(id), edges(Enum.map(tiles, &String.graphemes/1))}
  end

  @regex ~r/Tile (?<id>\d+):/
  defp extract_id(id), do: Regex.named_captures(@regex, id)["id"]
  defp product(ids), do: Enum.reduce(Enum.map(ids, &String.to_integer/1), &(&1 * &2))

  defp edges(tiles) do
    transposed = transpose(tiles)

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
