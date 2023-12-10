defmodule AdventOfCode.Y2016.Day06 do
  @moduledoc """
  --- Day 6: Signals and Noise ---
  Problem Link: https://adventofcode.com/2016/day/6
  Difficulty: xs
  Tags: sequence optimization
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2016, 6)

  def run(input \\ input()) do
    input = parse(input)

    {do_run(input, :max), do_run(input, :min)}
  end

  def do_run(input, type), do: Enum.map_join(input, &frequency(&1, type))

  def parse(input) do
    len = input |> Transformers.lines() |> hd() |> String.length()

    input
    |> String.replace("\n", "")
    |> String.replace("\r", "")
    |> String.codepoints()
    |> Enum.with_index()
    |> Enum.map(fn {k, idx} -> {k, rem(idx, len)} end)
    |> Enum.group_by(fn {_, group} -> group end, fn {v, _} -> v end)
    |> Map.values()
    |> Enum.map(&Enum.join/1)
  end

  defp frequency(word, by) do
    word
    |> String.codepoints()
    |> Enum.group_by(& &1)
    |> Map.values()
    |> Enum.min_max_by(&length/1)
    |> then(fn {min, max} -> hd((by == :max && max) || min) end)
  end
end
