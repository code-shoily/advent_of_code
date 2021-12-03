defmodule AdventOfCode.Y2021.Day03 do
  @moduledoc """
  --- Day 3: Binary Diagnostic ---
  Problem Link: https://adventofcode.com/2021/day/3
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 3

  def run_1 do
    input!()
    |> parse()
    |> transpose()
    |> bit_frequencies()
    |> epsilon_gamma()
    |> Tuple.product()
  end

  def run_2, do: input!() |> parse() |> life_support_rating()

  def parse(data), do: data |> String.split("\n") |> Enum.map(&String.graphemes/1)

  defp transpose(data), do: data |> Enum.zip() |> Enum.map(&Tuple.to_list/1)

  defp bit_frequencies(data) do
    data
    |> Enum.map(&Enum.frequencies/1)
    |> Enum.reduce([], fn
      %{"0" => lo, "1" => hi}, acc when lo >= hi -> [{"0", "1"} | acc]
      _, acc -> [{"1", "0"} | acc]
    end)
  end

  defp to_integer_by(encoded_data, index) do
    encoded_data
    |> Enum.map_join(&elem(&1, index))
    |> String.reverse()
    |> String.to_integer(2)
  end

  defp epsilon_gamma(encoded_data) do
    {to_integer_by(encoded_data, 0), to_integer_by(encoded_data, 1)}
  end

  defp life_support_rating(data), do: o2(data, 0) * co2(data, 0)

  defp o2([result], _), do: to_integer(result)

  defp o2(data, idx) do
    value = frequent_by(data, idx, :o2)

    o2(
      Enum.filter(data, &(Enum.at(&1, idx) == value)),
      idx + 1
    )
  end

  defp co2([result], _), do: to_integer(result)

  defp co2(data, idx) do
    value = frequent_by(data, idx, :co2)

    co2(
      Enum.filter(data, &(Enum.at(&1, idx) == value)),
      idx + 1
    )
  end

  defp frequent_by(data, idx, strategy) do
    data
    |> Enum.map(&Enum.at(&1, idx))
    |> Enum.frequencies()
    |> then(fn
      %{"0" => lo, "1" => hi} when lo > hi -> (strategy == :o2 && "0") || "1"
      _ -> (strategy == :o2 && "1") || "0"
    end)
  end

  defp to_integer(result), do: result |> Enum.join() |> String.to_integer(2)
end
