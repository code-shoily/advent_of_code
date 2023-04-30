defmodule AdventOfCode.Y2020.Day10 do
  @moduledoc """
  --- Day 10: Adapter Array ---
  Problem Link: https://adventofcode.com/2020/day/10
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2020, 10)

  def run(input \\ input()) do
    input = Enum.sort([0 | Transformers.int_lines(input)], :desc)
    {multiply_1_3(rates(input)), count(input)}
  end

  defp rates(data) do
    my_rate = Enum.max(data) + 3
    data = MapSet.new(data)

    Stream.unfold(0, fn
      n when n > my_rate ->
        nil

      n ->
        range = MapSet.new((n + 1)..(n + 3))
        jolt = MapSet.intersection(data, range) |> Enum.to_list()
        if Enum.empty?(jolt), do: diffs(my_rate, n), else: diffs(Enum.min(jolt), n)
    end)
  end

  defp diffs(a, b) when a - b == 1, do: {{a, 1, 0}, a}
  defp diffs(a, b) when a - b == 3, do: {{a, 0, 1}, a}
  defp diffs(_, _), do: nil

  defp multiply_1_3(r),
    do: apply(&Kernel.*/2, Enum.reduce(r, [0, 0], fn {_, a, b}, [x, y] -> [x + a, y + b] end))

  defp count(data), do: count(data, %{(hd(data) + 3) => 1})
  defp count([], cache), do: cache[0]

  defp count([h | rst], m),
    do: count(rst, Map.put(m, h, Enum.sum(Enum.map(1..3, &Map.get(m, h + &1, 0)))))
end
