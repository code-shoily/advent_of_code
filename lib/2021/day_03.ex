defmodule AdventOfCode.Y2021.Day03 do
  @moduledoc """
  --- Day 3: Binary Diagnostic ---
  Problem Link: https://adventofcode.com/2021/day/3
  """
  alias AdventOfCode.Helpers.InputReader

  import AdventOfCode.Helpers.Transformers

  def input, do: InputReader.read_from_file(2021, 3)

  def run(input \\ input()) do
    input = input |> lines() |> Enum.map(&digits/1)
    {Tuple.product(epsilon_gamma(input)), life_support_rating(input)}
  end

  defp epsilon_gamma(data) do
    data
    |> transpose()
    |> bits()
    |> then(&{to_int(&1, 0), to_int(&1, 1)})
  end

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

    data
    |> Enum.filter(&(Enum.at(&1, idx) == value))
    |> o2(idx + 1)
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
