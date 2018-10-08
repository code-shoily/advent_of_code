defmodule AdventOfCode.Y2016.Day2 do
  use AdventOfCode.Data.InputReader, year: 2016, day: 2

  @initial_position 5

  defp valid?(number), do: number <= 0 or number > 9

  defp next(cur, "L") when rem(cur, 3) != 1, do: cur - 1
  defp next(cur, "R") when rem(cur, 3) != 0, do: cur + 1
  defp next(cur, "U"), do: cur - 3
  defp next(cur, "D"), do: cur + 3
  defp next(_, _), do: -1

  defp parse([], res), do: res
  defp parse([h | t], []), do: parse(t, [find(h, @initial_position)])
  defp parse([h | t], [x | _] = res), do: parse(t, [find(h, x) | res])

  defp find([], cur), do: cur

  defp find([h | t], cur) do
    next_key = next(cur, h)

    cond do
      valid?(next_key) -> find(t, cur)
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
