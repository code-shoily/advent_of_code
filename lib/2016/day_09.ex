defmodule AdventOfCode.Y2016.Day09 do
  @moduledoc """
  --- Day 9: Explosives in Cyberspace ---
  Problem Link: https://adventofcode.com/2016/day/9
  """
  use AdventOfCode.Helpers.InputReader, year: 2016, day: 9

  @init [0, 1]
  @open "("
  @close ")"
  @regex ~r[\((\d+)x(\d+)\)]

  def run_1(input), do: decompress_v1(input)
  def run_2(input), do: decompress_v2(input)

  defp decompress_v1(seq), do: decompress_v1(seq, @init, [], 0)
  defp decompress_v1("", _, _, out), do: out

  defp decompress_v1(@open <> xs, [0, _], ms, out),
    do: decompress_v1(xs, @init, [@open], out + length(ms))

  defp decompress_v1(@close <> xs, @init, ms, out) do
    [@close | ms]
    |> Enum.reverse()
    |> Enum.join("")
    |> then(&Regex.run(@regex, &1, capture: :all_but_first))
    |> Enum.map(&String.to_integer/1)
    |> then(&decompress_v1(xs, &1, [], out))
  end

  defp decompress_v1(<<_::bytes-size(1)>> <> xs, [0, _], [], out),
    do: decompress_v1(xs, @init, [], out + 1)

  defp decompress_v1(<<x::bytes-size(1)>> <> xs, @init, ms, out),
    do: decompress_v1(xs, @init, [x | ms], out)

  defp decompress_v1(<<_::bytes-size(1)>> <> xs, [cov, rep], [], out),
    do: decompress_v1(xs, [cov - 1, rep], [], out + rep)

  @regex ~r[(.*?)\((\d+)x(\d+)\)(.*)]
  defp decompress_v2(seq, count \\ 0) do
    case Regex.run(@regex, seq, capture: :all_but_first) do
      nil ->
        count + String.length(seq)

      [prefix, cov, rep, xs] ->
        {cov, rep} = {String.to_integer(cov), String.to_integer(rep)}

        decompress_v2(
          String.slice(xs, cov..-1),
          count + String.length(prefix) + decompress_v2(String.slice(xs, 0, cov), 0) * rep
        )
    end
  end
end
