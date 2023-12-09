defmodule AdventOfCode.Y2015.Day25 do
  @moduledoc """
  --- Day 25: Let It Snow ---
  Problem Link: https://adventofcode.com/2015/day/25
  Difficulty: m
  Tags: table-lookup modular-arithmetic
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2015, 25)

  def run(input \\ input()) do
    solution =
      input
      |> parse()
      |> index_of()
      |> nth_code()

    {solution, "🎉"}
  end

  def index_of({row, 1}) do
    1..row
    |> Enum.reduce(%{0 => 1}, fn row, codes ->
      Map.merge(codes, %{row => Map.get(codes, row - 1) + (row - 1)})
    end)
    |> Map.get(row)
  end

  def index_of({row, col}) do
    2..col
    |> Enum.reduce(%{1 => index_of({row, 1})}, fn col, codes ->
      Map.merge(codes, %{col => Map.get(codes, col - 1) + row + (col - 1)})
    end)
    |> Map.get(col)
  end

  def nth_code(1), do: 20_151_125

  def nth_code(n) do
    Enum.reduce(2..n, nth_code(1), fn _, code ->
      rem(code * 252_533, 33_554_393)
    end)
  end

  def parse(data) do
    ~r/\D+(\d+)\D+(\d+)/
    |> Regex.run(data, capture: :all_but_first)
    |> Enum.map(&String.to_integer/1)
    |> List.to_tuple()
  end
end
