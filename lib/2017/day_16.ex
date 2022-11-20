defmodule AdventOfCode.Y2017.Day16 do
  @moduledoc """
  --- Day 16: Permutation Promenade ---
  Problem Link: https://adventofcode.com/2017/day/16
  """
  use AdventOfCode.Helpers.InputReader, year: 2017, day: 16

  alias AdventOfCode.Helpers.Transformers

  @dancer_count 16

  def run(input \\ input!()) do
    dancers = ?a..?z |> Enum.take(@dancer_count) |> to_string()
    moves = parse(input)

    {dance(dancers, moves), perform_billionth(dancers, moves)}
  end

  defp perform_billionth(dancers, moves) do
    count = rem(1_000_000_000, repeats_after(dancers, moves))

    Enum.reduce(1..count, dancers, fn _, dancers ->
      dance(dancers, moves)
    end)
  end

  def repeats_after(dancers, moves), do: repeats_after(dancers, moves, 0, MapSet.new())

  def repeats_after(dancers, moves, count, done) do
    dancers = dance(dancers, moves)

    (MapSet.member?(done, dancers) && count) ||
      repeats_after(dancers, moves, count + 1, MapSet.put(done, dancers))
  end

  def parse(data \\ input!()) do
    data
    |> Transformers.words(",")
    |> Enum.map(&parse_move/1)
  end

  defp parse_move("s" <> size) do
    {:spin, String.to_integer(size)}
  end

  @regex ~r"^(.+)\/(.+)$"
  defp parse_move("x" <> rest) do
    {:exchange,
     @regex
     |> Regex.run(rest, capture: :all_but_first)
     |> then(&(&1 |> Enum.map(fn i -> String.to_integer(i) end) |> Enum.sort()))}
  end

  defp parse_move("p" <> rest) do
    {:partner, Enum.sort(Regex.run(@regex, rest, capture: :all_but_first))}
  end

  def dance(dancers, []), do: dancers

  def dance(dancers, [move | moves]) do
    # So many repeating words :D
    dance(dance(dancers, move), moves)
  end

  def dance(dancers, {:exchange, [a, b]}) do
    Enum.join([
      String.slice(dancers, 0, a),
      String.at(dancers, b),
      String.slice(dancers, a + 1, b - a - 1),
      String.at(dancers, a),
      String.slice(dancers, (b + 1)..-1)
    ])
  end

  def dance(dancers, {:spin, a}) do
    String.slice(dancers, -a..-1) <> String.slice(dancers, 0..(-a - 1))
  end

  def dance(dancers, {:partner, [a, b]}) do
    dancers
    |> String.replace(a, "_")
    |> String.replace(b, a)
    |> String.replace("_", b)
  end
end
