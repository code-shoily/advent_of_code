defmodule AdventOfCode.Y2020.Day5 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/5
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 5

  def run_1, do: input!() |> process() |> Enum.max()
  def run_2, do: input!() |> process() |> my_id()
  def run, do: {run_1(), run_2()}

  def process(input), do: String.split(input, "\n") |> ids()

  def walk("F" <> pass, lo, hi), do: walk(pass, lo, mid(lo, hi))
  def walk("B" <> pass, lo, hi), do: walk(pass, mid(lo, hi) + 1, hi)
  def walk(pass, row, row) when pass != "", do: {row, walk(pass, 0, 7)}

  def walk("L" <> pass, lo, hi), do: walk(pass, lo, mid(lo, hi))
  def walk("R" <> pass, lo, hi), do: walk(pass, mid(lo, hi) + 1, hi)
  def walk("", column, column), do: column

  defp mid(lo, hi), do: div(lo + hi, 2)
  defp uid({row, col}), do: row * 8 + col

  defp ids(data), do: Enum.map(data, &uid(walk(&1, 0, 127)))

  defp my_id(ids) do
    sorted_ids = Enum.sort(ids)

    sorted_ids
    |> Enum.zip(tl(sorted_ids))
    |> Enum.filter(fn {a, b} -> b - a != 1 end)
    |> (fn [{_, b}] -> b - 1 end).()
  end
end
