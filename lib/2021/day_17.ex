defmodule AdventOfCode.Y2021.Day17 do
  @moduledoc """
  --- Day 17: Trick Shot ---
  Problem Link: https://adventofcode.com/2021/day/17
  Difficulty: m
  Tags: simulation physics
  """

  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2021, 17)

  def run(input \\ input()) do
    {xr, yr} = parse(input)

    # Physics: For any positive vy, the probe will return to y=0 with velocity -vy.
    # The next step will bring it to y = -(vy + 1).
    # To hit the trench, -(vy + 1) must be >= the bottom of the trench (yr.first).
    # So max(vy) = abs(yr.first) - 1.
    max_vy = abs(yr.first) - 1
    max_height = div(max_vy * (max_vy + 1), 2)

    hits =
      for vx <- 1..xr.last,
          vy <- yr.first..abs(yr.first),
          hits?({vx, vy}, {xr, yr}),
          do: {vx, vy}

    {max_height, Enum.count(hits)}
  end

  defp hits?({vx, vy}, {xr, yr}) do
    Stream.iterate({0, 0, vx, vy}, fn {x, y, vx, vy} ->
      {x + vx, y + vy, max(0, vx - 1), vy - 1}
    end)
    |> Enum.reduce_while(nil, fn {x, y, _vx, vy}, _ ->
      cond do
        x in xr and y in yr -> {:halt, true}
        x > xr.last or (y < yr.first and vy < 0) -> {:halt, false}
        true -> {:cont, nil}
      end
    end)
  end

  def parse(data \\ input()) do
    [x1, x2, y1, y2] =
      Regex.scan(~r/-?\d+/, data)
      |> List.flatten()
      |> Enum.map(&String.to_integer/1)

    {x1..x2, min(y1, y2)..max(y1, y2)}
  end
end
