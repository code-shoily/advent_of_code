defmodule AdventOfCode.Y2020.Day12 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/12
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 12

  def run_1, do: input!() |> process() |> move() |> distance()

  def process(input), do: input |> String.split("\n") |> parse()

  defp parse([]), do: []

  defp parse([<<dir::utf8>> <> val | rest]),
    do: [{String.to_existing_atom(<<dir>>), String.to_integer(val)} | parse(rest)]

  defp move(dirs), do: move(dirs, {0, 0}, :E)

  defp move([], pos, _), do: pos
  defp move([{:N, val} | rest], {x, y}, f), do: move(rest, {x, y + val}, f)
  defp move([{:S, val} | rest], {x, y}, f), do: move(rest, {x, y - val}, f)
  defp move([{:E, val} | rest], {x, y}, f), do: move(rest, {x + val, y}, f)
  defp move([{:W, val} | rest], {x, y}, f), do: move(rest, {x - val, y}, f)

  defp move([{:F, val} | rest], {x, y}, f),
    do: move([{f, val} | rest], {x, y}, f)

  defp move([{lr, val} | rest], {x, y}, f) when lr in [:L, :R],
    do: move([{turn(f, lr, val), 0} | rest], {x, y}, turn(f, lr, val))

  defp turn(:N, :L, val), do: do_turn([:W, :S, :E], val)
  defp turn(:N, :R, val), do: do_turn([:E, :S, :W], val)
  defp turn(:S, :L, val), do: do_turn([:E, :N, :W], val)
  defp turn(:S, :R, val), do: do_turn([:W, :N, :E], val)
  defp turn(:E, :L, val), do: do_turn([:N, :W, :S], val)
  defp turn(:E, :R, val), do: do_turn([:S, :W, :N], val)
  defp turn(:W, :L, val), do: do_turn([:S, :E, :N], val)
  defp turn(:W, :R, val), do: do_turn([:N, :E, :S], val)

  defp do_turn(directions, val), do: Enum.at(directions, div(val, 90) - 1)

  defp distance({x, y}), do: abs(x) + abs(y)
end
