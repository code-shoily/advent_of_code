defmodule AdventOfCode.Y2025.Day12 do
  @moduledoc """
  --- Day 12: Christmas Tree Farm ---
  Problem Link: https://adventofcode.com/2025/day/12
  Difficulty: m
  Tags: geometry
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2025, 12)

  def run(input \\ input()) do
    {shapes, regions} = parse_v2(input)

    {run_1(shapes, regions), :done}
  end

  defp run_1(shapes, regions) do
    Enum.count(regions, fn {w, h, requirements} ->
      sum_area =
        requirements
        |> Enum.with_index()
        |> Enum.map(fn {count, idx} -> count * Enum.at(shapes, idx).area end)
        |> Enum.sum()

      sum_area <= w * h
    end)
  end

  def parse_v2(input) do
    lines = Transformers.lines(input)

    {shape_lines, region_lines} =
      Enum.split_with(lines, fn line -> !String.match?(line, ~r/^\d+x\d+:/) end)

    shapes =
      shape_lines
      |> Enum.chunk_while(
        [],
        fn line, acc ->
          if String.match?(line, ~r/^\d+:$/) do
            if acc == [], do: {:cont, [line]}, else: {:cont, Enum.reverse(acc), [line]}
          else
            {:cont, [line | acc]}
          end
        end,
        fn
          [] -> {:cont, []}
          acc -> {:cont, Enum.reverse(acc), []}
        end
      )
      |> Enum.map(fn [id_line | grid_lines] ->
        id = String.replace(id_line, ":", "") |> String.to_integer()
        area = grid_lines |> Enum.map(fn row -> String.count(row, "#") end) |> Enum.sum()
        %{id: id, area: area}
      end)

    regions =
      region_lines
      |> Enum.map(fn line ->
        [dim, counts_str] = String.split(line, ":", trim: true)
        [w, h] = String.split(dim, "x") |> Enum.map(&String.to_integer/1)
        counts = String.split(counts_str, " ", trim: true) |> Enum.map(&String.to_integer/1)
        {w, h, counts}
      end)

    {shapes, regions}
  end

  def parse(data \\ input()) do
    data
    |> Transformers.lines()
  end
end
