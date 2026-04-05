defmodule AdventOfCode.Y2018.Day09 do
  @moduledoc """
  --- Day 9: Marble Mania ---
  Problem Link: https://adventofcode.com/2018/day/9
  Difficulty: s
  Tags: sequence slow zipper
  """
  alias AdventOfCode.Algorithms.BiCircularList
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2018, 9)

  def run(input \\ input()) do
    {players, last_marble} = parse(input)

    task_1 = Task.async(fn -> solve(players, last_marble) end)
    task_2 = Task.async(fn -> solve(players, last_marble * 100) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def parse(data) do
    data
    |> Transformers.words()
    |> Enum.flat_map(fn word ->
      case Integer.parse(word) do
        {n, ""} -> [n]
        _ -> []
      end
    end)
    |> then(fn [players, last_marble] -> {players, last_marble} end)
  end

  def solve(players, last_marble) do
    %BiCircularList{current: 0}
    |> play(1, 1, players, last_marble, %{})
  end

  # Base case: marble > last_marble
  defp play(_, _, marble, _, last_marble, scores) when marble > last_marble do
    scores |> Map.values() |> Enum.max(fn -> 0 end)
  end

  defp play(state, player, marble, players, last_marble, scores) when rem(marble, 23) == 0 do
    state
    |> counter_clockwise(7)
    |> BiCircularList.pop()
    |> then(fn {score, state} ->
      play(
        state,
        player + 1,
        marble + 1,
        players,
        last_marble,
        Map.update(scores, rem(player, players), score + marble, &(&1 + score + marble))
      )
    end)
  end

  defp play(state, player, marble, players, last_marble, scores) do
    state
    |> BiCircularList.next()
    |> BiCircularList.insert(marble)
    |> play(player + 1, marble + 1, players, last_marble, scores)
  end

  defp counter_clockwise(%BiCircularList{} = bcl, n) do
    Enum.reduce(1..n, bcl, fn _, acc ->
      BiCircularList.previous(acc)
    end)
  end
end
