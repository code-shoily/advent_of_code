defmodule AdventOfCode.Y2018.Day09 do
  @moduledoc """
  --- Day 9: Marble Mania ---
  Problem Link: https://adventofcode.com/2018/day/9
  Difficulty: s
  Tags: slow revisit circular-linked-list
  """
  alias AdventOfCode.Algorithms.BiCircularList

  @players 473
  @last_marble 70_904

  def run do
    task_1 = Task.async(fn -> run_1() end)
    task_2 = Task.async(fn -> run_2() end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  def run_1 do
    %BiCircularList{current: 0}
    |> play(1, 1, @last_marble, %{})
    |> Enum.max_by(fn {_, v} -> v end)
    |> elem(1)
  end

  defp run_2 do
    %BiCircularList{current: 0}
    |> play(1, 1, @last_marble * 100, %{})
    |> Enum.max_by(fn {_, v} -> v end)
    |> elem(1)
  end

  def play(_, _, marble, marble, scores), do: scores

  def play(state, player, marble, last_marble, scores) when rem(marble, 23) == 0 do
    state
    |> counter_clockwise(7)
    |> BiCircularList.pop()
    |> then(fn {score, state} ->
      play(
        state,
        player + 1,
        marble + 1,
        last_marble,
        Map.update(scores, rem(player, @players), score + marble, &(&1 + score + marble))
      )
    end)
  end

  def play(state, player, marble, last_marble, scores) do
    state
    |> BiCircularList.next()
    |> BiCircularList.insert(marble)
    |> play(player + 1, marble + 1, last_marble, scores)
  end

  defp counter_clockwise(%BiCircularList{} = bcl, n) do
    Enum.reduce(1..n, bcl, fn _, acc ->
      BiCircularList.previous(acc)
    end)
  end
end
