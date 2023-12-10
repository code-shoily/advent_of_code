defmodule AdventOfCode.Y2023.Day09 do
  @moduledoc """
  --- Day 9: Mirage Maintenance ---
  Problem Link: https://adventofcode.com/2023/day/9
  Difficulty: xs
  Tags: sequence reduction
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 9)

  def run(input \\ input()) do
    input = parse(input)

    {extrapolate_by(input, &forward/1), extrapolate_by(input, &backward/1)}
  end

  def extrapolate_by(histories, direction_fn),
    do: Enum.reduce(histories, 0, fn x, acc -> acc + direction_fn.(x) end)

  def parse(data \\ input()) do
    for line <- Transformers.lines(data), do: Transformers.int_words(line)
  end

  defp forward(data) do
    extrapolate(data, [data], fn acc ->
      acc
      |> Enum.map(&List.last/1)
      |> Enum.sum()
    end)
  end

  defp backward(data) do
    extrapolate(data, [data], fn acc ->
      acc
      |> Enum.map(&hd/1)
      |> Enum.reduce(fn x, acc ->
        x - acc
      end)
    end)
  end

  defp extrapolate(histories, acc, direction_fn) do
    case MapSet.size(MapSet.new(histories)) do
      1 ->
        direction_fn.(acc)

      _ ->
        histories
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.map(fn [a, b] -> b - a end)
        |> then(fn record -> extrapolate(record, [record | acc], direction_fn) end)
    end
  end
end
