defmodule AdventOfCode.Y2020.Day15 do
  @moduledoc """
  --- Day 15: Rambunctious Recitation ---
  Problem Link: https://adventofcode.com/2020/day/15
  """
  def run_1, do: [6, 19, 0, 5, 7, 13, 1] |> play(2020)
  def run_2, do: [6, 19, 0, 5, 7, 13, 1] |> play(30_000_000)

  def play(input, stop) do
    tab = :ets.new(:tab, [])
    true = :ets.insert(tab, Enum.with_index(input, 1))
    speak({List.last(input), tab, length(input)}, stop)
  end

  defp speak({last, _hist, stop}, stop), do: last

  defp speak({last, hist, turn}, stop) do
    case :ets.lookup(hist, last) do
      [] ->
        true = :ets.insert(hist, {last, turn})
        speak({0, hist, turn + 1}, stop)

      [{_, prev_turn}] ->
        true = :ets.insert(hist, {last, turn})
        speak({turn - prev_turn, hist, turn + 1}, stop)
    end
  end
end
