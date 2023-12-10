defmodule AdventOfCode.Y2022.Day25 do
  @moduledoc """
  --- Day 25: Full of Hot Air ---
  Problem Link: https://adventofcode.com/2022/day/25
  Difficulty: m
  Tags: number-system
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 25)

  def run(input \\ input()) do
    {
      to_snafu(
        for snafu <- Transformers.lines(input), reduce: 0 do
          acc -> acc + to_decimal(snafu)
        end
      ),
      "🎉"
    }
  end

  defp to_decimal(snafu) do
    snafu
    |> String.graphemes()
    |> Enum.reverse()
    |> Enum.with_index()
    |> Enum.reduce(0, fn
      {"-", i}, acc -> -1 * 5 ** i + acc
      {"=", i}, acc -> -2 * 5 ** i + acc
      {n, i}, acc -> String.to_integer(n) * 5 ** i + acc
    end)
  end

  defp to_snafu(decimal), do: Enum.map_join(to_snafu(decimal, []), &to_string/1)

  defp to_snafu(0, xs), do: xs

  defp to_snafu(decimal, xs) do
    case rem(decimal, 5) do
      3 -> to_snafu(div(decimal + 2, 5), ["=" | xs])
      4 -> to_snafu(div(decimal + 1, 5), ["-" | xs])
      num -> to_snafu(div(decimal, 5), [num | xs])
    end
  end
end
