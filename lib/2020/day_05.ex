defmodule AdventOfCode.Y2020.Day05 do
  @moduledoc """
  --- Day 5: Binary Boarding ---
  Problem Link: https://adventofcode.com/2020/day/5
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 5)

  def run(input \\ input()) do
    input = ids(Transformers.lines(input))
    {Enum.max(input), my_id(input)}
  end

  def walk("F" <> pass, lo, hi), do: walk(pass, lo, mid(lo, hi))
  def walk("B" <> pass, lo, hi), do: walk(pass, mid(lo, hi) + 1, hi)
  def walk(pass, row, row) when pass != "", do: {row, walk(pass, 0, 7)}

  def walk("L" <> pass, lo, hi), do: walk(pass, lo, mid(lo, hi))
  def walk("R" <> pass, lo, hi), do: walk(pass, mid(lo, hi) + 1, hi)
  def walk("", column, column), do: column

  defp ids(data), do: Enum.map(data, &uid(walk(&1, 0, 127)))

  defp mid(lo, hi), do: div(lo + hi, 2)
  defp uid({row, col}), do: row * 8 + col

  defp my_id(ids) do
    sorted_ids = Enum.sort(ids)

    sorted_ids
    |> Enum.zip(tl(sorted_ids))
    |> Keyword.filter(fn {a, b} -> b - a != 1 end)
    |> then(fn [{_, b}] -> b - 1 end)
  end
end
