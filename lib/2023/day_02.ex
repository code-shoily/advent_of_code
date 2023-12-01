defmodule AdventOfCode.Y2023.Day02 do
  @moduledoc """
  --- Day 2: Cube Conundrum ---
  Problem Link: https://adventofcode.com/2023/day/2
  Difficulty: s
  Tags: reduction
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 2)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  defp run_1(input) do
    for game <- input, reduce: 0 do
      acc ->
        acc + winning_game_id(game)
    end
  end

  defp run_2(input) do
    for {_, {r, g, b}} <- input, reduce: 0 do
      acc ->
        acc + r * g * b
    end
  end

  def parse(data \\ input()) do
    for line <- Transformers.lines(data) do
      game_data(line)
    end
  end

  @reg ~r/Game (\d+): (.*)/
  defp game_data(line) do
    [[id, rest]] = Regex.scan(@reg, line, capture: :all_but_first)
    {id, rgb_count(rest)}
  end

  defp winning_game_id({_, {r, g, b}}) when r > 12 or g > 13 or b > 14, do: 0
  defp winning_game_id({id, _}), do: String.to_integer(id)

  defp rgb_count(chunk) do
    chunk
    |> String.split("; ")
    |> Enum.map(&parse_colour_configs/1)
    |> then(&min_config/1)
  end

  defp parse_colour_configs(line) do
    line
    |> String.split(", ")
    |> Enum.map(&get_colours/1)
    |> then(&merge_colours/1)
  end

  defp min_config(colours) do
    Enum.reduce(colours, {0, 0, 0}, fn {r, g, b}, {ra, ga, ba} ->
      {
        (r > ra && r) || ra,
        (g > ga && g) || ga,
        (b > ba && b) || ba
      }
    end)
  end

  defp merge_colours(colours),
    do: Enum.reduce(colours, {0, 0, 0}, fn {a, b, c}, {x, y, z} -> {a + x, b + y, c + z} end)

  defp get_colours(tuple) do
    case String.split(tuple, " ") do
      [q, "red"] -> {int(q), 0, 0}
      [q, "green"] -> {0, int(q), 0}
      [q, "blue"] -> {0, 0, int(q)}
    end
  end

  defp int(v), do: String.to_integer(v)
end
