defmodule AdventOfCode.Y2020.Day20 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/20
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 20

  import AdventOfCode.Helpers.Transformers

  def run_1, do: input!() |> process() |> corners() |> product()
  def process(input), do: String.split(input, "\n\n") |> Enum.map(&parse/1) |> Map.new()

  defp parse(tile) do
    [id | tiles] = String.split(tile, "\n")
    {extract_id(id), edges(Enum.map(tiles, &String.graphemes/1))}
  end

  defp extract_id(id), do: Regex.named_captures(~r/Tile (?<id>\d+):/, id)["id"]
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
end
