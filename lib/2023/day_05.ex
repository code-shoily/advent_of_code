defmodule AdventOfCode.Y2023.Day05 do
  @moduledoc """
  --- Day 5: If You Give A Seed A Fertilizer ---
  Problem Link: https://adventofcode.com/2023/day/5
  Difficulty: xl
  Tags: internval-tree gb-tree
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 5)

  def run(input \\ input()) do
    {seeds, mapping} = parse(input)

    {run_1(seeds, mapping), run_2(seeds |> corrected_seeds(), mapping)}
  end

  defp run_1(seeds, mappings) do
    seeds
    |> Enum.map(fn seed -> Enum.reduce(mappings, seed, &get_destination/2) end)
    |> Enum.min()
  end

  defp run_2(seeds, mappings) do
    mappings
    |> Enum.reduce(seeds, fn mapping, sources ->
      Enum.flat_map(sources, &get_destinations(mapping, &1))
    end)
    |> Enum.min()
    |> elem(0)
  end

  def parse(data \\ input()) do
    data
    |> Transformers.sections()
    |> then(&{parse_seeds(hd(&1)), parse_mappings(tl(&1))})
  end

  defp parse_seeds(seeds),
    do: with([_ | seeds] <- String.split(seeds, " "), do: Enum.map(seeds, &String.to_integer(&1)))

  defp parse_mappings(mappings) do
    for mapping <- mappings do
      with [_, seeds] <- String.split(mapping, " map:\n") do
        seeds
        |> String.split("\n")
        |> Enum.map(fn line ->
          line
          |> String.split(" ")
          |> Enum.map(&String.to_integer/1)
        end)
        |> Enum.reduce(:gb_trees.empty(), fn [destination, source, len], tree ->
          :gb_trees.insert(
            {source, source + len - 1},
            {destination, destination + len - 1},
            tree
          )
        end)
      end
    end
  end

  defp corrected_seeds(seeds) do
    seeds
    |> Stream.chunk_every(2)
    |> Enum.map(fn [source, len] -> {source, source + len - 1} end)
  end

  defp get_destination(mapping, source) do
    mapping
    |> :gb_trees.iterator()
    |> Stream.unfold(fn iter ->
      case :gb_trees.next(iter) do
        :none -> nil
        {source_range, destination_range, iter} -> {{source_range, destination_range}, iter}
      end
    end)
    |> Enum.find({{0, 0}, {0, 0}}, fn {{a, b}, _} -> source in a..b end)
    |> then(fn {{a, _}, {c, _}} -> c + source - a end)
  end

  defp get_destinations(mapping, range) do
    mapping
    |> :gb_trees.iterator()
    |> get_destinations(range, [])
  end

  defp get_destinations(iter, {a, b}, acc) do
    build_destinations(:gb_trees.next(iter), {a, b}, acc)
  end

  # ¯\_(ツ)_/¯
  defp build_destinations(:none, {a, b}, acc), do: [{a, b} | acc]

  #            a -- b
  # si -- sf
  defp build_destinations({{_, sf}, {_, _}, iter}, {a, b}, acc) when sf < a,
    do: get_destinations(iter, {a, b}, acc)

  # a -- b
  #         si -- sf
  defp build_destinations({{si, _}, {_, _}, _}, {a, b}, acc) when si > b, do: [{a, b} | acc]

  #      a -- b
  #  si -------- sf
  defp build_destinations({{si, sf}, {di, _}, _}, {a, b}, acc) when si <= a and sf >= b,
    do: [{a - si + di, b - si + di} | acc]

  #  a ---------- b
  #  si -- sf
  defp build_destinations({{si, sf}, {di, df}, iter}, {a, b}, acc) when si == a and sf < b,
    do: get_destinations(iter, {sf + 1, b}, [{di, df} | acc])

  # a ------- b
  #    si -- sf
  defp build_destinations({{si, sf}, {di, df}, _}, {a, b}, acc) when si > a and sf == b,
    do: [{a, si - 1}, {di, df} | acc]

  # a ------------ b
  #    si -- sf
  defp build_destinations({{si, sf}, {di, df}, iter}, {a, b}, acc) when si > a and sf < b,
    do: get_destinations(iter, {sf + 1, b}, [{a, si - 1}, {di, df} | acc])

  #      a ----- b
  # si ---- sf
  defp build_destinations({{si, sf}, {di, df}, iter}, {a, b}, acc) when si < a and sf < b,
    do: get_destinations(iter, {sf + 1, b}, [{a - si + di, df} | acc])

  # a ----- b
  #     si ---- sf
  defp build_destinations({{si, sf}, {_, _}, _}, {a, b}, acc) when si > a and sf > b,
    do: [{a, b} | acc]
end
