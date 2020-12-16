defmodule AdventOfCode.Y2020.Day15 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2020/day/15
  """
  def run_1, do: [6, 19, 0, 5, 7, 13, 1] |> prepare() |> speak(2020)
  def run_2, do: [6, 19, 0, 5, 7, 13, 1] |> prepare() |> speak(30_000_000)

  def prepare(dat) do
    {List.last(dat), for({v, i} <- Enum.with_index(dat, 1), into: %{}, do: {v, [i]}), length(dat)}
  end

  defp speak({val, _, stop}, stop), do: val

  defp speak({val, mem, turn}, stop) do
    case {Map.get(mem, val), turn + 1} do
      {[_], t} -> speak({0, Map.update(mem, 0, [t], &[t | &1]), t}, stop)
      {[a, b | _], t} -> speak({a - b, Map.update(mem, a - b, [t], &[t | &1]), t}, stop)
    end
  end
end
