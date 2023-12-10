defmodule AdventOfCode.Y2017.Day16 do
  @moduledoc """
  --- Day 16: Permutation Promenade ---
  Problem Link: https://adventofcode.com/2017/day/16
  Difficulty: m
  Tags: op-code count
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @dancers to_string(Enum.take(?a..?z, 16))

  def input, do: InputReader.read_from_file(2017, 16)

  def run(input \\ input()) do
    moves = parse(input)

    {dance(@dancers, moves), perform_billionth(@dancers, moves)}
  end

  defp perform_billionth(dancers, moves) do
    count = rem(1_000_000_000, repeats_after(dancers, moves))
    Enum.reduce(1..count, dancers, fn _, dancers -> dance(dancers, moves) end)
  end

  def parse(data \\ input()) do
    for m <- Transformers.words(data, ","), do: parse_move(m)
  end

  @re ~r"^(.+)\/(.+)$"
  defp parse_move("s" <> size), do: {:spin, String.to_integer(size)}

  defp parse_move("p" <> rest),
    do: {:partner, Enum.sort(Regex.run(@re, rest, capture: :all_but_first))}

  defp parse_move("x" <> rest) do
    {:exchange,
     @re
     |> Regex.run(rest, capture: :all_but_first)
     |> then(&Enum.sort(Enum.map(&1, fn i -> String.to_integer(i) end)))}
  end

  defp dance(dancers, []), do: dancers

  defp dance(dancers, [move | moves]) do
    # So many repeating words :D
    dance(dance(dancers, move), moves)
  end

  defp dance(dancers, {:exchange, [a, b]}) do
    Enum.join([
      String.slice(dancers, 0, a),
      String.at(dancers, b),
      String.slice(dancers, a + 1, b - a - 1),
      String.at(dancers, a),
      String.slice(dancers, (b + 1)..-1)
    ])
  end

  defp dance(dancers, {:spin, a}) do
    String.slice(dancers, -a..-1) <> String.slice(dancers, 0..(-a - 1))
  end

  defp dance(dancers, {:partner, [a, b]}) do
    dancers
    |> String.replace(a, "_")
    |> String.replace(b, a)
    |> String.replace("_", b)
  end

  defp repeats_after(dancers, moves), do: repeats_after(dancers, moves, 0, MapSet.new())

  defp repeats_after(dancers, moves, count, done) do
    dancers = dance(dancers, moves)

    (MapSet.member?(done, dancers) && count) ||
      repeats_after(dancers, moves, count + 1, MapSet.put(done, dancers))
  end
end
