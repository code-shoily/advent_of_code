defmodule AdventOfCode.Y2015.Day12 do
  @moduledoc """
  --- Day 12: JSAbacusFramework.io ---
  Problem Link: https://adventofcode.com/2015/day/12
  Difficulty: xs
  Tags: json
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2015, 12)

  def run(input \\ input()) do
    {run_1(input), run_2(input)}
  end

  def run_1(input) do
    input
    |> String.graphemes()
    |> parse_json([])
    |> compute()
  end

  def run_2(input) do
    input
    |> JSON.decode!()
    |> total()
  end

  @numbers "-0123456789"
  @delim " "

  def number?(s), do: String.contains?(@numbers, s)

  def parse_json([], result), do: result

  def parse_json([h | t], []) do
    (number?(h) && parse_json(t, [h])) || parse_json(t, [@delim])
  end

  def parse_json([h | t], [x | xs] = res) do
    (number?(h) && parse_json(t, [x <> h | xs])) || parse_json(t, [@delim | res])
  end

  def compute(parsed) do
    parsed
    |> Enum.join()
    |> String.split()
    |> Enum.sum_by(&String.to_integer/1)
  end

  def total(obj) when is_map(obj) do
    nodes = Map.values(obj)
    ("red" in nodes && 0) || total(nodes)
  end

  def total(obj) when is_list(obj) do
    obj
    |> Enum.sum_by(fn
      value when is_map(value) or is_list(value) -> total(value)
      value when is_integer(value) -> value
      _ -> 0
    end)
  end
end
