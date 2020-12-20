defmodule AdventOfCode.Y2020.Day20 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/20
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 20

  def run_1, do: input!() |> process() |> corner_tiles() |> product()

  def process(input \\ input!()) do
    input
    |> String.split("\n\n")
    |> Enum.map(&parse_tile/1)
    |> Enum.into(%{})
  end

  defp parse_tile(tile) do
    [id | tiles] = String.split(tile, "\n")
    {extract_id(id), edges(Enum.map(tiles, &String.graphemes/1))}
  end

  defp extract_id(id), do: Regex.named_captures(~r/Tile (?<id>\d+):/, id)["id"]

  defp product(ids) do
    ids
    |> Enum.map(&String.to_integer/1)
    |> Enum.reduce(&Kernel.*/2)
  end

  defp edges(tiles) do
    transposed = transpose(tiles)

    Enum.map(
      [hd(tiles), List.last(tiles), hd(transposed), List.last(transposed)],
      fn edge -> MapSet.new([Enum.join(edge), Enum.join(Enum.reverse(edge))]) end
    )
  end

  defp corner_tiles(data) do
    data
    |> all_edges()
    |> find_unique(data)
    |> Enum.reject(&is_nil/1)
  end

  defp all_edges(data) do
    data
    |> Map.values()
    |> Enum.flat_map(& &1)
    |> Enum.frequencies()
    |> Enum.filter(fn {_, c} -> c == 1 end)
    |> Enum.map(&elem(&1, 0))
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

  defp transpose(m) do
    case m do
      [[] | _] -> []
      m -> [Enum.map(m, &hd/1) | transpose(Enum.map(m, &tl/1))]
    end
  end
end
