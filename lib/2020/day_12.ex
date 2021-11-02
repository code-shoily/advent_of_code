defmodule AdventOfCode.Y2020.Day12 do
  @moduledoc """
  --- Day 12: Rain Risk ---
  Problem Link: https://adventofcode.com/2020/day/12
  """
  use AdventOfCode.Helpers.InputReader, year: 2020, day: 12

  def run_1, do: input!() |> parse() |> go() |> distance()
  def run_2, do: input!() |> parse() |> waypoints() |> distance()

  def parse(input), do: input |> String.split("\n") |> parse_actions()

  defp parse_actions([]), do: []

  defp parse_actions([<<dir::utf8>> <> val | xs]),
    do: [{String.to_existing_atom(<<dir>>), String.to_integer(val)} | parse_actions(xs)]

  defp go(dirs), do: go(dirs, {0, 0}, :E)
  defp go([], pos, _), do: pos
  defp go([{:N, val} | xs], {x, y}, f), do: go(xs, {x, y + val}, f)
  defp go([{:S, val} | xs], {x, y}, f), do: go(xs, {x, y - val}, f)
  defp go([{:E, val} | xs], {x, y}, f), do: go(xs, {x + val, y}, f)
  defp go([{:W, val} | xs], {x, y}, f), do: go(xs, {x - val, y}, f)
  defp go([{:F, val} | xs], {x, y}, f), do: go([{f, val} | xs], {x, y}, f)

  defp go([{lr, val} | xs], {x, y}, f),
    do: go([{turn(f, lr, val), 0} | xs], {x, y}, turn(f, lr, val))

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

  defp waypoints(data), do: waypoints(data, [{0, 0}, {10, 1}])
  defp waypoints([], [ship, _]), do: ship
  defp waypoints([rule | xs], positions), do: waypoints(xs, positions(rule, positions))

  defp positions({:N, val}, [{x, y}, {wx, wy}]), do: [{x, y}, {wx, wy + val}]
  defp positions({:S, val}, [{x, y}, {wx, wy}]), do: [{x, y}, {wx, wy - val}]
  defp positions({:E, val}, [{x, y}, {wx, wy}]), do: [{x, y}, {wx + val, wy}]
  defp positions({:W, val}, [{x, y}, {wx, wy}]), do: [{x, y}, {wx - val, wy}]
  defp positions({:F, val}, [{x, y}, {wx, wy}]), do: [{x + wx * val, y + wy * val}, {wx, wy}]
  defp positions({lr, val}, [{x, y}, {wx, wy}]), do: [{x, y}, turn_waypoint({wx, wy}, lr, val)]

  defp turn_waypoint({wx, wy}, _, 0), do: {wx, wy}
  defp turn_waypoint({wx, wy}, :L, val), do: turn_waypoint({-wy, wx}, :L, val - 90)
  defp turn_waypoint({wx, wy}, :R, val), do: turn_waypoint({wy, -wx}, :R, val - 90)
end
