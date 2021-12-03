defmodule AdventOfCode.Y2021.Day03 do
  @moduledoc """
  --- Day 3: Binary Diagnostic ---
  Problem Link: https://adventofcode.com/2021/day/3
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 3

  def run_1, do: input!() |> parse() |> epsilon_gamma() |> Tuple.product()
  def run_2, do: input!() |> parse() |> life_support_rating()

  def parse(data), do: data |> String.split("\n") |> Enum.map(&to_ints/1)
  defp to_ints(data), do: data |> String.graphemes() |> Enum.map(&String.to_integer/1)
  defp epsilon_gamma(data), do: data |> transposed() |> then(&{to_int(&1, 0), to_int(&1, 1)})
  defp transposed(data), do: data |> Enum.zip() |> Enum.map(&Tuple.to_list/1) |> bits()
  defp life_support_rating(data), do: o2(data, 0) * co2(data, 0)

  defp bits(data) do
    data
    |> Enum.map(&Enum.frequencies/1)
    |> Enum.reduce([], fn
      %{0 => lo, 1 => hi}, acc when lo >= hi -> [{0, 1} | acc]
      _, acc -> [{1, 0} | acc]
    end)
  end

  defp o2([result], _), do: to_int(result)

  defp o2(data, idx) do
    value = frequent_by(data, idx, :o2)

    o2(Enum.filter(data, &(Enum.at(&1, idx) == value)), idx + 1)
  end

  defp co2([result], _), do: to_int(result)

  defp co2(data, idx) do
    value = frequent_by(data, idx, :co2)

    co2(Enum.filter(data, &(Enum.at(&1, idx) == value)), idx + 1)
  end

  defp frequent_by(data, idx, kind) do
    data
    |> Enum.map(&Enum.at(&1, idx))
    |> Enum.frequencies()
    |> then(fn
      %{0 => lo, 1 => hi} when lo > hi -> (kind == :o2 && 0) || 1
      _ -> (kind == :o2 && 1) || 0
    end)
  end

  defp to_int(data), do: data |> Enum.join() |> String.to_integer(2)
  defp to_int(data, idx), do: data |> Enum.reverse() |> Enum.map(&elem(&1, idx)) |> to_int()
end
