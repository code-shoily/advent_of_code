defmodule AdventOfCode.Y2016.Day2 do
  use AdventOfCode.Data.InputReader, year: 2016, day: 2

  def next(cur, "L") when rem(cur, 3) != 1, do: cur - 1
  def next(cur, "R") when rem(cur, 3) != 0, do: cur + 1
  def next(cur, "U"), do: cur - 3
  def next(cur, "D"), do: cur + 3
  def next(_, _), do: -1

  def parse([], res), do: res
  def parse([h | t], []), do: parse(t, [find(h, 5)])
  def parse([h | t], [x | _] = res), do: parse(t, [find(h, x) | res])

  def find([], cur), do: cur

  def find([h | t], cur) do
    next_key = next(cur, h)

    cond do
      next_key <= 0 or next_key > 9 -> find(t, cur)
      true -> find(t, next_key)
    end
  end

  def run do
    input!()
    |> String.split("\n")
    |> Enum.map(&String.graphemes/1)
    |> parse([])
    |> Enum.reverse()
    |> Enum.join()
  end
end
